package org.dwcj.component.textcombobox.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.textcombobox.TextComboBox;
import org.dwcj.component.textcombobox.events.TextComboBoxEditModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class TextComboBoxEditModifyEventSink {
    
    private ArrayList<Consumer<TextComboBoxEditModifyEvent>> targets = new ArrayList<>();
    private final TextComboBox textComboBox;
    private BBjControl bbjctrl;


    @SuppressWarnings({"static-access"})
    public TextComboBoxEditModifyEventSink(TextComboBox cb) {
        this.textComboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxEditModifyEventSink(TextComboBox cb, Consumer<TextComboBoxEditModifyEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void editModifyEvent(BBjEditModifyEvent ev) { //NOSONAR
        TextComboBoxEditModifyEvent dwcEv = new TextComboBoxEditModifyEvent(this.textComboBox);
        Iterator<Consumer<TextComboBoxEditModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextComboBoxEditModifyEvent> callback) { targets.add(callback); }


}
