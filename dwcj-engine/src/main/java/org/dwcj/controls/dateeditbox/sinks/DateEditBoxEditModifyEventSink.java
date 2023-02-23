package org.dwcj.controls.dateeditbox.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.dateeditbox.DateEditBox;
import org.dwcj.controls.dateeditbox.events.DateEditBoxEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class DateEditBoxEditModifyEventSink {

    private ArrayList<Consumer<DateEditBoxEditModifyEvent>> targets;
    private final DateEditBox dateEditBox;

    @SuppressWarnings({"static-access"})
    public DateEditBoxEditModifyEventSink(DateEditBox dateBox) {

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

    public DateEditBoxEditModifyEventSink(DateEditBox dateBox, Consumer<DateEditBoxEditModifyEvent> callback) {

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
        DateEditBoxEditModifyEvent dwcEv = new DateEditBoxEditModifyEvent(this.dateEditBox);
        Iterator<Consumer<DateEditBoxEditModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<DateEditBoxEditModifyEvent> callback) {
        targets.add(callback);
    }
}
