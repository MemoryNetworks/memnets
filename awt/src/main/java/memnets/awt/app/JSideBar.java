package memnets.awt.app;

import memnets.awt.SelectionModelAwt;
import memnets.awt.util.ToggleAction;
import memnets.core.Engine;
import memnets.core.Library;
import memnets.core.ModelBuilder;
import memnets.model.Param;
import memnets.model.Signal;
import memnets.model.Trial;
import memnets.ui.Skin;
import memnets.utils.SelectionModel;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.Optional;

import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class JSideBar extends JPanel {
    private static final long serialVersionUID = 4L;
    final protected Engine engine;
    final protected JTabbedPane tabbedPane = new JTabbedPane();
    final protected JList<ModelBuilder> builderList = new JList<>();
    final DefaultListModel<ModelBuilder> builderModel = new DefaultListModel<>();
    final protected JToolBar toolBar = new JToolBar();
    final protected JLabel notification = new JLabel();
    final protected JList<Param> paramList = new JList<>();
    final protected JSlider paramSlider = new JSlider();
    final protected JList<Trial> trialList = new JList<>();
    final protected JList<Signal> inputList = new JList<>();
    final protected JList<Skin<?, ?>> skinList = new JList<>();
    final protected int width = 350;

    public JSideBar(Engine engine) {
        this.engine = engine;
        this.setLayout(new BorderLayout());
        this.add(toolBar, BorderLayout.NORTH);
        this.add(tabbedPane, BorderLayout.CENTER);
        this.add(notification, BorderLayout.SOUTH);

        this.setPreferredSize(new Dimension(width, 350));
        notification.setPreferredSize(new Dimension(width, 25));
        toolBar.setPreferredSize(new Dimension(width, 40));
        tabbedPane.setPreferredSize(new Dimension(width, 285));
        paramSlider.setPreferredSize(new Dimension(width, 60));

        toolBar.add(resetAction);
        playAction.addToggleListener(engine::setPlaying);
        engine.addBooleanListener("playing", playAction::setSelected);
        toolBar.add(playAction.bind(new JToggleButton()));

        this.addList("Library", builderList, Optional.empty());
        this.addList("Trials", trialList, Optional.of(engine.trialsModel()));
        JScrollPane paramScroll = this.addList("Params", paramList, Optional.of(engine.paramsModel()), false);
        tabbedPane.add("Params", new JPanel(new BorderLayout()) {
            {
                add(paramSlider, BorderLayout.NORTH);
                add(paramScroll, BorderLayout.CENTER);
            }
        });
        this.addList("Skins", skinList, Optional.of(engine.skinsModel()));
        this.addList("Inputs", inputList, Optional.of(engine.inputModel()));


        builderList.setModel(builderModel);
        builderList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        builderList.addListSelectionListener(e -> {
            if (validSelect(e)) {
                ModelBuilder b = builderList.getSelectedValue();
                if (b != engine.model().builder()) {
                    engine.setBuilder(b, true);
                }
            }
        });

        int pScale = 10;
        paramSlider.setPaintTicks(true);
        paramSlider.setSnapToTicks(false);
        // todo: labels are not correct b/c adjusted to be ints
//        paramSlider.setPaintLabels(true);

        engine.paramsModel().addSelectionListener(value -> {
            paramSlider.setEnabled(false);
            if (value != null) {
                paramSlider.setMinimum((int) (value.min() * pScale));
                paramSlider.setMaximum((int) (value.max() * pScale));
                paramSlider.setValue((int) (value.value() * pScale));
                paramSlider.setMajorTickSpacing((paramSlider.getMaximum() - paramSlider.getMinimum()) / 2);
                paramSlider.setMinorTickSpacing(paramSlider.getMajorTickSpacing() / 2);
                paramSlider.setEnabled(true);
                paramSlider.repaint();
            }
            return;
        });
        paramSlider.addChangeListener(sld -> {
            if (paramSlider.isEnabled()) {
                double value = paramSlider.getValue() / 10.0;
                engine.paramsModel().getSelected().setValue(value);
                paramSlider.setToolTipText("" + value);
                paramList.repaint();
            }
        });
    }

    public void showMsg(String text) {
        this.notification.setText(text);
    }

    public void addLibrary(Library lib) {
        for (ModelBuilder b : lib.getBuilders()) {
            builderModel.addElement(b);
        }
    }

    protected <T> JScrollPane addList(String name, JList<T> list, Optional<SelectionModel<T>> model) {
        return addList(name, list, model, true);
    }

    protected <T> JScrollPane addList(String name, JList<T> list, Optional<SelectionModel<T>> model, boolean add) {
        JScrollPane sp = new JScrollPane(list);
        sp.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);

        if (add)
            tabbedPane.addTab(name, sp);

        model.ifPresent(m -> {
            SelectionModelAwt<T> awt = new SelectionModelAwt<T>(m);
            list.setModel(awt.toListModel());
            list.setSelectionModel(awt.toListSelectionModel());
        });
        return sp;
    }

    protected boolean validSelect(ListSelectionEvent lse) {
        return !lse.getValueIsAdjusting() && lse.getFirstIndex() >= 0;
    }

    protected ToggleAction playAction = new ToggleAction("Play", null) {
        @Override
        protected void selectionPerformed(boolean selected) {
            System.out.println("play sel: " + selected);
        }
    };
    protected Action resetAction = new AbstractAction("Reset", null) {
        @Override
        public void actionPerformed(ActionEvent e) {
            engine.fireReset();
        }
    };
}
