package org.dwcj.controls.slider.sinks;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;

import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.slider.Slider;
import org.dwcj.controls.slider.events.SliderOnControlScrollEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class SliderOnControlScrollEventSink {

    private final ArrayList<Consumer<SliderOnControlScrollEvent>> targets = new ArrayList<>();

    private final Slider slider;

    private BBjControl bbjControl;

    @SuppressWarnings({"static-access"})
    public SliderOnControlScrollEventSink(Slider slide){
        this.slider = slide;

        try{
            bbjControl = ControlAccessor.getDefault().getBBjControl(slide);
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CONTROL_SCROLL,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "onScrollEvent"),
                                   "onEvent");

        } catch(Exception e){
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java:S1172")
    public void onScrollEvent(BBjControlScrollEvent ev){ //NOSONAR

        SliderOnControlScrollEvent dwcEv = new SliderOnControlScrollEvent(this.slider);
        Iterator<Consumer<SliderOnControlScrollEvent>> it = targets.iterator();
        while(it.hasNext()){
            it.next().accept(dwcEv);
        }
    }

    public void addCallback(Consumer<SliderOnControlScrollEvent> callback){
        targets.add(callback);
    }
    
}
