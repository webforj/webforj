package org.dwcj.component.radiobutton.sinks;

import com.basis.bbj.proxies.event.BBjCheckOffEvent;
import com.basis.bbj.proxies.event.BBjCheckOnEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;

import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.radiobutton.RadioButton;
import org.dwcj.component.radiobutton.events.RadioButtonCheckEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public final class RadioButtonCheckEventSink {
    
    private final ArrayList<Consumer<RadioButtonCheckEvent>> targets = new ArrayList<>();

    private final RadioButton radioButton;

    private BBjControl bbjControl;

    private static final String ON_EVENT = "onEvent";

    @SuppressWarnings({"static-access"})
    public RadioButtonCheckEventSink(RadioButton rb){
        this.radioButton = rb;

        try{
            bbjControl = ControlAccessor.getDefault().getBBjControl(rb);
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_OFF,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOffEvent"),
                                   ON_EVENT);
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_ON,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOnEvent"),
                                   ON_EVENT);

        } catch(Exception e){
            Environment.logError(e);
        }
    }
    
    @SuppressWarnings({"static-access"})
    public RadioButtonCheckEventSink(RadioButton rb, Consumer<RadioButtonCheckEvent> target){
        this.targets.add(target);
        this.radioButton = rb;

        try{
            bbjControl = ControlAccessor.getDefault().getBBjControl(rb);
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_OFF,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOffEvent"),
                                   ON_EVENT);
            bbjControl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_ON,
                                   Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOnEvent"),
                                   ON_EVENT);

        } catch(Exception e){
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java:S1172")
    public void checkOffEvent(BBjCheckOffEvent ev) { //NOSONAR
        RadioButtonCheckEvent dwcEv = new RadioButtonCheckEvent(this.radioButton, false);
        Iterator<Consumer<RadioButtonCheckEvent>> it = targets.iterator();
        while(it.hasNext()){
            it.next().accept(dwcEv);
        }
    }

    @SuppressWarnings("java:S1172")
    public void checkOnEvent(BBjCheckOnEvent ev) { //NOSONAR
        RadioButtonCheckEvent dwcEv = new RadioButtonCheckEvent(this.radioButton, true);
        Iterator<Consumer<RadioButtonCheckEvent>> it = targets.iterator();
        while(it.hasNext()){
            it.next().accept(dwcEv);
        }
    }

    public void addCallback(Consumer<RadioButtonCheckEvent> callback) {
        targets.add(callback);
    }

}
