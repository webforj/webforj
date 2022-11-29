package org.dwcj.events;

import org.dwcj.controls.Slider;

public class SliderOnControlScrollEvent implements IDwcEvent {
    
    private final Slider control;

    public SliderOnControlScrollEvent(Slider slider){ this.control = slider; }

    @Override
    public Slider getControl() { return control; }
}
