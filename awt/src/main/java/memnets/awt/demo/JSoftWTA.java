package memnets.awt.demo;

import memnets.models.StandardLibrary;

public class JSoftWTA {
    public static void main(String[] args) {
        JDemo demo = new JDemo(StandardLibrary.swta());
        demo.launch(args);
    }
}