package org.dwcj.component.combobox.events;
import org.dwcj.component.ComponentEvent;
import org.dwcj.component.combobox.TextComboBox;

public class TextComboBoxCloseEvent implements ComponentEvent{
    
    private final TextComboBox control;

    public TextComboBoxCloseEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }


}
