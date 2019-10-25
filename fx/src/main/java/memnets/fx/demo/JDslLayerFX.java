package memnets.fx.demo;

import memnets.core.ModelBuilder;
import memnets.fx.app.AppBaseFX;
import memnets.model.Layer;
import memnets.model.Trial;

public class JDslLayerFX extends AppBaseFX {
    @Override
    public ModelBuilder startUpBuilder() {
        return ModelBuilder.create("DSL layers", b ->
        {
            int size = 1024;
            double freq = 3.0;
            double dampen = -0.05;

            Layer x = b.Layer(size, "x");
            Layer dx = b.Layer(size, "dx", dampen);
            dx.ui().skip(); // hide in UI

            dx.linkTo(x);  // x = dx
            x.linkTo(dx).setW(-freq * freq);

            Trial trial = b.Trial(10 * 3600, "Phase Shift");
            trial.setOnReset(() ->
            {
                double mag = 4.0;
                double phaseShift = 2.0 * Math.PI / x.length();
                for (int i = 0; i < x.length(); i++) {
                    x.update(i, mag * Math.cos(i * phaseShift));
                    dx.update(i, mag * -freq * Math.sin(i * phaseShift));
                }
            });
        });
    }

    public static void main(String[] args) {
        launch(JDslLayerFX.class, args);
    }
}