package org.dwcj.component.datefield.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.datefield.DateField;
import org.dwcj.component.datefield.event.DateFieldModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class DateEditBoxEditModifyEventSink {

    private ArrayList<Consumer<DateFieldModifyEvent>> targets;
    private final DateField dateEditBox;

    @SuppressWarnings({"static-access"})
    public DateEditBoxEditModifyEventSink(DateField dateBox) {

        this.targets = new ArrayList<>();
        this.dateEditBox = dateBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(dateBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public DateEditBoxEditModifyEventSink(DateField dateBox, Consumer<DateFieldModifyEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.dateEditBox = dateBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(dateBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        DateFieldModifyEvent dwcEv = new DateFieldModifyEvent(this.dateEditBox);
        Iterator<Consumer<DateFieldModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<DateFieldModifyEvent> callback) {
        targets.add(callback);
    }
}
