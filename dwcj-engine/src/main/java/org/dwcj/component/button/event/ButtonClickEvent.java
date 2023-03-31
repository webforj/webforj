package org.dwcj.component.button.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.button.Button;

public final class ButtonClickEvent implements ComponentEvent {
    private final Button control;

    public ButtonClickEvent(Button cButton) {
        this.control = cButton;
    }

    @Override
    public Button getControl() {
        return control;
    }

    public String toString() {
        return "Event: ButtonPushed";
    }
}
