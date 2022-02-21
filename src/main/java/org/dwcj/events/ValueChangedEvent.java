package org.dwcj.events;

import org.dwcj.controls.AbstractDwcControl;

public class ValueChangedEvent implements IDwcEvent {

    private final AbstractDwcControl control;
    private final Double value;

    public ValueChangedEvent(AbstractDwcControl ctrl, Double val) {
        this.control = ctrl;
        this.value = val;
    }

    @Override
    public AbstractDwcControl getControl() {
        return control;
    }

    public Double getValue() {
        return value;
    }
}
