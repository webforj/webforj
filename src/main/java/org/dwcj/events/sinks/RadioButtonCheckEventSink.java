package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjCheckOffEvent;
import com.basis.bbj.proxies.event.BBjCheckOnEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;

import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.RadioButton;
import org.dwcj.events.RadioButtonCheckEvent;


import java.util.function.Consumer;


public final class RadioButtonCheckEventSink {
    
    private final Consumer<RadioButtonCheckEvent> target;

    private final RadioButton radioButton;

    private BBjControl bbjControl;

    @SuppressWarnings({"static-access"})
    public RadioButtonCheckEventSink(RadioButton rb, Consumer<RadioButtonCheckEvent> target){
        this.target = target;
        this.radioButton = rb;

        try{
            bbjControl = ControlAccessor.getDefault().getBBjControl(rb);
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_OFF,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOffEvent"),
                                   "onEvent");
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_ON,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOnEvent"),
                                   "onEvent");

        } catch(Exception e){
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java:S1172")
    public void checkOffEvent(BBjCheckOffEvent ev) { //NOSONAR
        RadioButtonCheckEvent dwcEv = new RadioButtonCheckEvent(this.radioButton, false);
        target.accept(dwcEv);
    }

    @SuppressWarnings("java:S1172")
    public void checkOnEvent(BBjCheckOnEvent ev) { //NOSONAR
        RadioButtonCheckEvent dwcEv = new RadioButtonCheckEvent(this.radioButton, true);
        target.accept(dwcEv);
    }


}
