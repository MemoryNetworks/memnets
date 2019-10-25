package memnets.ml;

public enum PredictType {
    TP, FP, TN, FN;

    public boolean isWrong() {
        return !isCorrect();
    }

    public boolean isCorrect() {
        return this == TP || this == TN;
    }

    public String desc() {
        String text = "";
        if (this == TP)
            text = "True +";
        else if (this == TN)
            text = "True -";
        else if (this == FP)
            text = "False +";
        else if (this == FN)
            text = "False -";
        return text;
    }
}