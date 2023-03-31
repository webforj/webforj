package org.dwcj.component.combobox.sinks;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.combobox.TextComboBox;
import org.dwcj.component.combobox.event.TextComboBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class TextComboBoxChangeEventSink {

    private ArrayList<Consumer<TextComboBoxChangeEvent>> targets = new ArrayList<>();

    private final TextComboBox textComboBox;

    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public TextComboBoxChangeEventSink(TextComboBox cb) {
        this.textComboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxChangeEventSink(TextComboBox cb, Consumer<TextComboBoxChangeEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void changeEvent(BBjListChangeEvent ev) { //NOSONAR
        TextComboBoxChangeEvent dwcEv = new TextComboBoxChangeEvent(this.textComboBox);
        Iterator<Consumer<TextComboBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextComboBoxChangeEvent> callback) { targets.add(callback); }
    
}
