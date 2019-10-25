package memnets.model;

public enum Viz {
    Dead, // sim will not use, filtered out of nodes
    Skip, // factory will never create
    Hide, // factory will add if elementFilter changed
    Fade, // element can fade out completely (for big nets)
    Default, // fade out but not vanish
    Focus, // will never fade out
    User // no fade out, highlight drawn
}