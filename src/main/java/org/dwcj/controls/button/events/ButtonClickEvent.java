package org.dwcj.controls.button.events;

import org.dwcj.controls.button.Button;
import org.dwcj.interfaces.ControlEvent;

public final class ButtonClickEvent implements ControlEvent {
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
