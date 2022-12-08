package org.dwcj.controls.button.events;

import org.dwcj.controls.button.Button;
import org.dwcj.interfaces.IDwcEvent;

public final class ButtonPushEvent implements IDwcEvent {
    private final Button control;

    public ButtonPushEvent(Button cButton) {
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
