package memnets.awt.app;

import memnets.awt.J2DSkin;
import memnets.awt.JSkinBase;
import memnets.core.ModelUIBase;
import memnets.core.SkinBuiltEvent;
import memnets.model.Display;
import memnets.model.Tick;
import memnets.ui.Skin;
import memnets.ui.TickableUI;
import memnets.utils.JavaUtils;

import javax.swing.*;
import java.awt.*;

public class JModel extends ModelUIBase<JComponent, JSkinBase> {
    public JModel() {
        super(JavaUtils.classTag(JSkinBase.class));
    }

    @Override
    public Skin<?, Color> defaultSkin() {
        return new J2DSkin();
    }

    @Override
    public void tick(Tick te) {
        ui.repaint();
    }

    JPanel ui = new JPanel() {
        {
            setBackground(Color.BLACK);
            setFocusable(true);
            setLayout(null);
            setDoubleBuffered(true);
        }

        // hack to grab graphics2D before real tick
        @Override
        protected void paintComponent(Graphics g) {
            final Graphics2D g2 = (Graphics2D) g;
            skinTyped().setGraphics2D(g2);
            skinTyped().drawBackground(getWidth(), getHeight());
            // NOTE : tricky bit here to call super.tick since overriding...
            // now tickables can use g2 canvas
            JModel.super.tick(system().now());
        }
    };

    @Override
    public void init() {
        super.init();
        ui.setPreferredSize(new Dimension(Display.resolution().getWidth(), Display.resolution().getHeight()));
    }

    @Override
    public void rebuild() {
        ui.removeAll();
        skinTyped().initStyle();
        super.rebuild();
        // always last
        publish(new SkinBuiltEvent(getSkin()));
    }

    @Override
    public void removeT(TickableUI<JComponent> jt) {
        super.removeT(jt);
        jt.getNode().ifPresent(c -> ui.remove(c));
    }

    /**
     * NOTE: scala protected is Java public.  don't like that at all
     */
    @Override
    public void addHelper(TickableUI<JComponent> tfx) {
        tfx.getNode().ifPresent(comp -> ui.add(comp));
    }
}