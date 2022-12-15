package org.dwcj.controls.slider.events;

import org.dwcj.controls.slider.Slider;
import org.dwcj.interfaces.ControlEvent;

public class SliderOnControlScrollEvent implements ControlEvent {
    
    private final Slider control;

    public SliderOnControlScrollEvent(Slider slider){ this.control = slider; }

    @Override
    public Slider getControl() { return control; }
}
