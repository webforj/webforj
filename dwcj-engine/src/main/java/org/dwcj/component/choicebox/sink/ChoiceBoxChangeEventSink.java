package org.dwcj.component.choicebox.sink;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.choicebox.ChoiceBox;
import org.dwcj.component.choicebox.event.ChoiceBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class ChoiceBoxChangeEventSink {

    private ArrayList<Consumer<ChoiceBoxChangeEvent>> targets;

    private final ChoiceBox comboBox;


    @SuppressWarnings({"static-access"})
    public ChoiceBoxChangeEventSink(ChoiceBox cb) {
        this.targets = new ArrayList<>();
        this.comboBox = cb;

        BBjControl bbjctrl = null;
        try{
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }
    
    @SuppressWarnings({"static-access"})
    public ChoiceBoxChangeEventSink(ChoiceBox cb, Consumer<ChoiceBoxChangeEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.comboBox = cb;

        BBjControl bbjctrl = null;
        try{
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void changeEvent(BBjListChangeEvent ev) { // NOSONAR
        ChoiceBoxChangeEvent dwcEv = new ChoiceBoxChangeEvent(this.comboBox);
        Iterator<Consumer<ChoiceBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ChoiceBoxChangeEvent> callback) { targets.add(callback); }
}
