package memnets.awt.app;

import memnets.awt.J2DSkin;
import memnets.core.*;
import memnets.model.Descriptable;
import memnets.model.Display;
import memnets.model.HD720;
import memnets.model.Signal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

abstract public class JAppBase implements AppUI, ActionListener {
    final protected Logger logger = LoggerFactory.getLogger(this.getClass().getName());
    protected ModelConfig _config;
    protected boolean showSideBar = true;

    public JAppBase() {
        logger.debug("ctor");
        _config = ModelConfig.create();
    }

    protected final JModel jModel = new JModel();
    protected final Engine engine = new Engine(jModel, this);
    protected final JSideBar sideBar = new JSideBar(engine);
    // NOTE: Windows Swing sometimes caps at 30fps???
    protected final Timer timer = new Timer(1000 / 60, this);
    protected final JFrame frame = new JFrame();

    final public void actionPerformed(ActionEvent e) {
        engine.tick(System.currentTimeMillis());
    }

    public ModelConfig config() {
        return _config;
    }

    public String title() {
        return "MemNets Swing Demo";
    }

    public void loadResources() {
    }

    public void startup(String[] args) {
        // todo: check startup args for HD
        // don't change after startup.  just scale...
        Display.setResolution(HD720.getResolution());

        jModel.init();

        final JFrame t = frame;
        t.setTitle(title());
        t.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        t.setContentPane(jModel.ui);
        jModel.ui.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        t.pack();
        t.setVisible(true);

        loadResources();

        config().setSkinFactory(() -> new J2DSkin());
        setBuilder(startUpBuilder());
        engine.setLoop(true);
        jModel.ui.setCursor(Cursor.getDefaultCursor());

        if (showSideBar) {
            final JFrame sbf = new JFrame("MemNets SideBar");
            sbf.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
            sbf.setContentPane(sideBar);
            sbf.pack();
            sbf.setVisible(true);
        }
        timer.setInitialDelay(200);
        timer.start();
    }

    public void process(EngineEvent e) {
        // Java version of Scala match is kind of nasty.  Suggestions welcome.
        switch (e.getClass().getSimpleName()) {
            case "ErrorEvent":
                ErrorEvent ee = (ErrorEvent) e;
                logger.error("error", ee.getEx());
                System.exit(0);
                return;
            case "SignalEvent":
                SignalEvent se = (SignalEvent) e;
                Signal sig = se.getSignal();
                if (se.on())
                    showMsg(sig);
                else if (sig.descOff() != "")
                    showMsg(sig.descOff());
                return;
            case "ResetEvent":
                ResetEvent re = (ResetEvent) e;
                showMsg("trial preparing: " + re.getTrial().description());
                return;
            case "GameIntroOverEvent":
                GameIntroOverEvent ge = (GameIntroOverEvent) e;
                showMsg("trial starting: " + jModel.trial().description());
                return;
            case "TrialDoneEvent":
                TrialDoneEvent te = (TrialDoneEvent) e;
                showMsg("trial done: " + te.getTrial().description());
                return;
            case "ProgressEvent":
                ProgressEvent pe = (ProgressEvent) e;
                logger.debug("progress: " + pe.getMsg());
                return;
            default:
                logger.debug("unprocessed event: " + e);
                return;
        }
        /* todo: port more functionality
        case SelectEvent(y) =>
            onSelection(y)
        case FullSceneEvent(fullFX)  =>
            val skinsModel = engineFX.skinsModel
            val skin = sysFX.skinTyped match {
            case fullSkin: FullSceneSkin =>
                logger.trace("fullScreenFX end event")
                fullSkin.prior
            case default =>
                logger.trace("fullSceneFX start event")
                val fullSkin = FullSceneSkin(fullFX, prior = default)
                skinsModel += fullSkin
                fullSkin
        }
        skinsModel.select(skin)
        logger.debug("skins : " + skinsModel.getItems.mkString(","))
        case default =>
            processHelper(default)
         */
    }

    public void setBuilder(ModelBuilder bld) {
        if (bld != null) {
            engine.setBuilder(bld, true);
            logger.debug("setting 1st builder");
            sideBar.builderList.setSelectedValue(bld, true);
        }
    }

    protected void showMsg(Descriptable desc) {
        if (desc != null && !engine.builderChanging())
            showMsg(desc.description());
    }

    protected void showMsg(String text) {
        logger.debug("showMsg: " + text);
        sideBar.showMsg(text);
    }
}
