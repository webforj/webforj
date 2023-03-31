package org.dwcj.component.field.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.field.TextBox;
import org.dwcj.component.field.event.TextBoxEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class TextBoxEditModifyEventSink {
    
    private ArrayList<Consumer<TextBoxEditModifyEvent>> targets;
    private final TextBox textBox;

    @SuppressWarnings({"static-access"})
    public TextBoxEditModifyEventSink(TextBox tBox) {

        this.targets = new ArrayList<>();
        this.textBox = tBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(tBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        TextBoxEditModifyEvent dwcEv = new TextBoxEditModifyEvent(this.textBox);
        Iterator<Consumer<TextBoxEditModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextBoxEditModifyEvent> callback) {
        targets.add(callback);
    }

}
