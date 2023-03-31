package org.dwcj.component.textarea.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.textarea.TextArea;
import org.dwcj.component.textarea.event.TextAreaModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class TextAreaModifyEventSink {

    private ArrayList<Consumer<TextAreaModifyEvent>> targets;
    private final TextArea multilineEdit;

    @SuppressWarnings({"static-access"})
    public TextAreaModifyEventSink(TextArea txtArea) {

        this.targets = new ArrayList<>();
        this.multilineEdit = txtArea;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(txtArea);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public TextAreaModifyEventSink(TextArea txtArea, Consumer<TextAreaModifyEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.multilineEdit = txtArea;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(txtArea);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        TextAreaModifyEvent dwcEv = new TextAreaModifyEvent(this.multilineEdit);
        Iterator<Consumer<TextAreaModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextAreaModifyEvent> callback) {
        targets.add(callback);
    }
}
