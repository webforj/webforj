package org.dwcj.component.slider.event;

import org.dwcj.component.slider.Slider;
import org.dwcj.interfaces.ControlEvent;

public class SliderOnControlScrollEvent implements ControlEvent {
    
    private final Slider control;

    public SliderOnControlScrollEvent(Slider slider){ this.control = slider; }

    @Override
    public Slider getControl() { return control; }
}
