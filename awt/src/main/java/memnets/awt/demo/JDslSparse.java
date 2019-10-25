package memnets.awt.demo;

import memnets.awt.JButtonSkin;
import memnets.core.ModelBuilder;
import memnets.model.Colorf;
import memnets.model.Osc;
import memnets.model.Param;
import memnets.model.Y;

public class JDslSparse {
    public static void main(String[] args) {
        JDemo demo = new JDemo(
                ModelBuilder.create(b ->
                {
                    double damping = 0.01;

                    Y y = b.Y("y");
                    Y x = b.Y("x", -damping);

                    Param freq = b.Param("frequency", 1.0, 0.5);
                    freq.setFunc(d -> {
                        double f = Osc.toFreq(d, y.getTau());
                        return -f * f;
                    });

                    y.linkTo(x);
                    x.linkTo(y, freq);

                    b.System().setOnTick(te -> {
                        if (te.t() % b.toSec(2) == 0) {
                            System.out.println("onTick every 2s");
                        }
                    });

                    b.Trial(b.toMin(1), "IC by Step");
                    // on = 0 sets initial condition (system forces duration = 0)
                    b.Step(y, 0, 10.0);

                    b.Trial(b.toMin(1), "IC by onReset")
                            .setOnReset(() -> y.update(10.0));

                    b.Trial(b.toMin(1), "sin input");
                    b.Sin(y, b.toSec(1), Osc.toPeriod(freq.getValue()), Math.PI, 0.5);

                    b.addSkin(b.Skin(skin -> {
                        skin.setBackColor(Colorf.gray(0.2));
                    }));
                    b.addSkin(new JButtonSkin());
                })
        );
        demo.launch(args);
    }
}


