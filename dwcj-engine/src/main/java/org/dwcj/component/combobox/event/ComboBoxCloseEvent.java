package org.dwcj.component.combobox.event;
import org.dwcj.component.ComponentEvent;
import org.dwcj.component.combobox.ComboBox;

public class ComboBoxCloseEvent implements ComponentEvent{
    
    private final ComboBox control;

    public ComboBoxCloseEvent(ComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }


}
