package memnets.fx.demo;

import memnets.core.ModelBuilder;
import memnets.fx.SkinFX;
import memnets.fx.app.AppBaseFX;
import memnets.model.Activation;
import memnets.model.Y;
import memnets.ui.Phase2D;
import memnets.ui.SkinImage;

public class JDslFunctionFX extends AppBaseFX {
    @Override
    public ModelBuilder startUpBuilder() {
        return ModelBuilder.create("DSL functions", b ->
        {
            Y prey = b.Y("Prey", 2.0);
            Y pred = b.Y("Pred", -1.0);

            prey.setTau(60.0);
            pred.setTau(60.0);

            prey.setActivation(Activation.Relu);
            pred.setActivation(Activation.Relu);
            // same as relu above, but setOut can take any double -> double
            // pred.setOut(d -> d < 0.0 ? 0.0 : d);

            prey.createF("-py * pd", t -> -prey.act() * pred.act());
            pred.createF("py * pd", t -> prey.act() * pred.act());

            Phase2D ph = b.Phase(pred, prey);
            ph.setScale(40.0);

            b.Step(pred, 0, 1.0);
            b.Step(prey, 0, 0.1);

            // add custom skin with background image
            b.addSkin(SkinFX.apply(x -> {
                x.setBackImage(SkinImage.TWO);
                x.setBackImageOn(true);
            }));
        });
    }

    public static void main(String[] args) {
        launch(JDslFunctionFX.class, args);
    }
}
