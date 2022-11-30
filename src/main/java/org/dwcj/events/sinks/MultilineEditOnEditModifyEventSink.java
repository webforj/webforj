package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.MultilineEdit;
import org.dwcj.events.MultilineEditOnEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class MultilineEditOnEditModifyEventSink {

    private ArrayList<Consumer<MultilineEditOnEditModifyEvent>> targets;
    private final MultilineEdit multilineEdit;

    @SuppressWarnings({"static-access"})
    public MultilineEditOnEditModifyEventSink(MultilineEdit MLEdit) {

        this.targets = new ArrayList<>();
        this.multilineEdit = MLEdit;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(MLEdit);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public MultilineEditOnEditModifyEventSink(MultilineEdit MLEdit, Consumer<MultilineEditOnEditModifyEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.multilineEdit = MLEdit;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(MLEdit);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        MultilineEditOnEditModifyEvent dwcEv = new MultilineEditOnEditModifyEvent(this.multilineEdit);
        Iterator<Consumer<MultilineEditOnEditModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<MultilineEditOnEditModifyEvent> callback) {
        targets.add(callback);
    }
}
