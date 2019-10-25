package memnets.awt.util;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.beans.PropertyChangeSupport;

abstract public class ToggleAction extends AbstractAction {
    private static final long serialVersionUID = 4L;
    protected PropertyChangeSupport _pcs = new PropertyChangeSupport(this);
    private boolean _selected = false;

    public boolean isSelected() {
        return _selected;
    }

    public void setSelected(boolean value) {
        boolean oldValue = this._selected;
        if (oldValue != value) {
            this._selected = value;
            selectionPerformed(value);
            this._pcs.firePropertyChange("selected", oldValue, value);
        }
    }

    public void addToggleListener(ToggleListener toggleListener) {
        this._pcs.addPropertyChangeListener("selected", pc -> toggleListener.selected((boolean) pc.getNewValue()));
    }

    public ToggleAction(String name, Icon icon) {
        super(name, icon);
    }

    @Override
    final public void actionPerformed(ActionEvent e) {
    }

    abstract protected void selectionPerformed(boolean selected);

    public JToggleButton bind(JToggleButton toggle) {
        toggle.setAction(this);
        toggle.setSelected(isSelected());
        toggle.addItemListener(ie -> setSelected(ItemEvent.SELECTED == ie.getStateChange()));
        this.addToggleListener(toggle::setSelected);
        return toggle;
    }
}
