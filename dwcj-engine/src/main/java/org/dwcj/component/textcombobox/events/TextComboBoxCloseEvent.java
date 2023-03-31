package org.dwcj.component.textcombobox.events;
import org.dwcj.component.textcombobox.TextComboBox;
import org.dwcj.interfaces.ComponentEvent;

public class TextComboBoxCloseEvent implements ComponentEvent{
    
    private final TextComboBox control;

    public TextComboBoxCloseEvent(TextComboBox cTextComboBox) {
        this.control = cTextComboBox;
    }

    @Override
    public TextComboBox getControl() { return control; }


}
