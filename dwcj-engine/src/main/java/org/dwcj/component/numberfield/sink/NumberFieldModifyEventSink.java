package org.dwcj.component.numberfield.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.numberfield.NumberField;
import org.dwcj.component.numberfield.event.NumberFieldModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class NumberFieldModifyEventSink {

    private ArrayList<Consumer<NumberFieldModifyEvent>> targets;
    private final NumberField numericBox;
    
    @SuppressWarnings({"static-access"})
    public NumberFieldModifyEventSink(NumberField numBox) {

        this.targets = new ArrayList<>();
        this.numericBox = numBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(numBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public NumberFieldModifyEventSink(NumberField numBox, Consumer<NumberFieldModifyEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.numericBox = numBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(numBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        NumberFieldModifyEvent dwcEv = new NumberFieldModifyEvent(this.numericBox);
        Iterator<Consumer<NumberFieldModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<NumberFieldModifyEvent> callback) {
        targets.add(callback);
    }
}
