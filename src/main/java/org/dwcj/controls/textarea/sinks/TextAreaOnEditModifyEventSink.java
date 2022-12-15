package org.dwcj.controls.textarea.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.textarea.TextArea;
import org.dwcj.controls.textarea.events.TextAreaOnEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class TextAreaOnEditModifyEventSink {

    private ArrayList<Consumer<TextAreaOnEditModifyEvent>> targets;
    private final TextArea multilineEdit;

    @SuppressWarnings({"static-access"})
    public TextAreaOnEditModifyEventSink(TextArea txtArea) {

        this.targets = new ArrayList<>();
        this.multilineEdit = txtArea;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(txtArea);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public TextAreaOnEditModifyEventSink(TextArea txtArea, Consumer<TextAreaOnEditModifyEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.multilineEdit = txtArea;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(txtArea);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        TextAreaOnEditModifyEvent dwcEv = new TextAreaOnEditModifyEvent(this.multilineEdit);
        Iterator<Consumer<TextAreaOnEditModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextAreaOnEditModifyEvent> callback) {
        targets.add(callback);
    }
}
