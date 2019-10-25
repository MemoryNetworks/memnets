package memnets.core;


import memnets.model.Osc;
import memnets.model.Param;
import memnets.model.Tick;
import memnets.model.Y;
import org.junit.Test;

/**
 * this isn't a test as much as a check on interoperability of Java API
 * more work to be done here...
 */
public class JSimTest {
    @Test
    public void coreApi() {

        ModelBuilder builder = ModelBuilder.create("Osc", b ->
        {
            Y y = b.Y("y");
            Y dy = b.Y("dy");
            Param freq = b.Param("freq", 2.0, 0.5);
            freq.setFunc(d -> {
                double f = Osc.toFreq(d, y.getTau());
                return -f * f;
            });
            dy.linkTo(y);
            y.linkTo(dy, freq);

            b.Trial("IC by Step");
            // on = 0 sets initial condition, always has dur = 0
            b.Step(y, 0, 6.0);

            // can set ICs in trial.onReset
            b.Trial("IC by onReset")
                    .setOnReset(() -> y.update(6.0));
        });

        BuiltModel cxt = builder.build(ModelConfig.create());
        Sim sim = cxt.buildSim(true);
        Tick tick = sim.nowTick();
        int n = sim.getSystem().sparse().getSize();
        for (int i = 0; i < 1000; i++) {
            sim.step();
            if (tick.t() < 5 || tick.t() % 10 == 0)
                System.out.println(tick.prettyPrint(n));
        }
        System.out.println("final");
        System.out.println(tick.prettyPrint(n));
        cxt.validator().tick(tick);
        sim.destroy();
        cxt.destroy();
    }
}
