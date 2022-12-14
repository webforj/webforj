package org.dwcj.controls.slider.events;

import org.dwcj.controls.slider.Slider;
import org.dwcj.interfaces.DwcEvent;

public class SliderOnControlScrollEvent implements DwcEvent {
    
    private final Slider control;

    public SliderOnControlScrollEvent(Slider slider){ this.control = slider; }

    @Override
    public Slider getControl() { return control; }
}
