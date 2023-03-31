package org.dwcj.component.slider.event;

import org.dwcj.component.slider.Slider;
import org.dwcj.interfaces.ComponentEvent;

public class SliderScrollEvent implements ComponentEvent {
    
    private final Slider control;

    public SliderScrollEvent(Slider slider){ this.control = slider; }

    @Override
    public Slider getControl() { return control; }
}
