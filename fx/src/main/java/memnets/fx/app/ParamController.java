package memnets.fx.app;

import javafx.fxml.FXML;
import javafx.scene.control.Slider;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import memnets.model.Param;

import java.util.ResourceBundle;

public class ParamController {
    @FXML
    ResourceBundle resources;
    // tag
    @FXML
    public Slider tieSlider;
    @FXML
    public TextField tiePrecTxt;
    @FXML
    public TableView<Param> tieTable;
    @FXML
    public AnchorPane tiePane;

    @FXML
    void initialize() {
    }
}
