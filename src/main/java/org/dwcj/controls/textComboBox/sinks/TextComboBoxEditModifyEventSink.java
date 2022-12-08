package org.dwcj.controls.textComboBox.sinks;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.textComboBox.TextComboBox;
import org.dwcj.controls.textComboBox.events.TextComboBoxEditModifyEvent;

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
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxEditModifyEventSink(TextComboBox cb, Consumer<TextComboBoxEditModifyEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
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
