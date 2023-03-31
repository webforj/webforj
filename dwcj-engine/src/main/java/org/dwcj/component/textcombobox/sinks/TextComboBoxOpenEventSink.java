package org.dwcj.component.textcombobox.sinks;

import com.basis.bbj.proxies.event.BBjListOpenEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.textcombobox.TextComboBox;
import org.dwcj.component.textcombobox.events.TextComboBoxOpenEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class TextComboBoxOpenEventSink {

    private ArrayList<Consumer<TextComboBoxOpenEvent>> targets = new ArrayList<>();
    private final TextComboBox textComboBox;
    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public TextComboBoxOpenEventSink(TextComboBox cb) {
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxOpenEventSink(TextComboBox cb, Consumer<TextComboBoxOpenEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void openEvent(BBjListOpenEvent ev) { //NOSONAR
        TextComboBoxOpenEvent dwcEv = new TextComboBoxOpenEvent(this.textComboBox);
        Iterator<Consumer<TextComboBoxOpenEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextComboBoxOpenEvent> callback) { targets.add(callback); }
    
}
