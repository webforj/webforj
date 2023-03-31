package org.dwcj.component.choicebox.sink;

import com.basis.bbj.proxies.event.BBjListOpenEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.component.choicebox.event.ChoiceBoxOpenEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class ChoiceBoxOpenEventSink {

    private ArrayList<Consumer<ChoiceBoxOpenEvent>> targets = new ArrayList<>();
    private final ComboBox comboBox;
    private BBjControl bbjctrl;


    @SuppressWarnings({"static-access"})
    public ChoiceBoxOpenEventSink(ComboBox cb) {
        this.comboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public ChoiceBoxOpenEventSink(ComboBox cb, Consumer<ChoiceBoxOpenEvent> callback) {
        this.targets.add(callback);
        this.comboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void openEvent(BBjListOpenEvent ev) { //NOSONAR
        ChoiceBoxOpenEvent dwcEv = new ChoiceBoxOpenEvent(this.comboBox);
        Iterator<Consumer<ChoiceBoxOpenEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ChoiceBoxOpenEvent> callback) { targets.add(callback); }


    
}
