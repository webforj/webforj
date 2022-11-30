package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.NumericBox;
import org.dwcj.events.NumericBoxEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class NumericBoxEditModifyEventSink {

    private ArrayList<Consumer<NumericBoxEditModifyEvent>> targets;
    private final NumericBox numericBox;
    
    @SuppressWarnings({"static-access"})
    public NumericBoxEditModifyEventSink(NumericBox numBox) {

        this.targets = new ArrayList<>();
        this.numericBox = numBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(numBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public NumericBoxEditModifyEventSink(NumericBox numBox, Consumer<NumericBoxEditModifyEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.numericBox = numBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(numBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        NumericBoxEditModifyEvent dwcEv = new NumericBoxEditModifyEvent(this.numericBox);
        Iterator<Consumer<NumericBoxEditModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<NumericBoxEditModifyEvent> callback) {
        targets.add(callback);
    }
}
