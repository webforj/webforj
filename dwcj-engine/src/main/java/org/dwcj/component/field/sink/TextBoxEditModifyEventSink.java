package org.dwcj.component.field.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.field.TextBox;
import org.dwcj.component.field.event.FieldModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class TextBoxEditModifyEventSink {
    
    private ArrayList<Consumer<FieldModifyEvent>> targets;
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
        FieldModifyEvent dwcEv = new FieldModifyEvent(this.textBox);
        Iterator<Consumer<FieldModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<FieldModifyEvent> callback) {
        targets.add(callback);
    }

}
