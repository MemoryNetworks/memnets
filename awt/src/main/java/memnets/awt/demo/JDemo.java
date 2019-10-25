package memnets.awt.demo;

import memnets.awt.app.JAppBase;
import memnets.core.ModelBuilder;
import memnets.models.DslExamples;
import memnets.models.StandardLibrary;

import javax.swing.*;

public class JDemo extends JAppBase {
    private ModelBuilder _startupBuilder;

    public JDemo(ModelBuilder builder) {
        this._startupBuilder = builder;
    }

    @Override
    public ModelBuilder startUpBuilder() {
        return _startupBuilder;
    }

    @Override
    public void loadResources() {
        System.out.println("loadResources");
        super.loadResources();
        this.sideBar.addLibrary(DslExamples.toJava());
        this.sideBar.addLibrary(StandardLibrary.toJava());
    }


    public void launch(String[] args) {
        SwingUtilities.invokeLater(() -> this.startup(args));
    }

}



