package org.dwcj.component.textfield.sink;

import com.basis.bbj.proxies.event.BBjEditModifyEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.textfield.TextField;
import org.dwcj.component.textfield.event.TextFieldModifyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class TextFieldModifyEventSink {
    
    private ArrayList<Consumer<TextFieldModifyEvent>> targets;
    private final TextField stringEditBox;


    @SuppressWarnings({"static-access"})
    public TextFieldModifyEventSink(TextField stringBox) {

        this.targets = new ArrayList<>();
        this.stringEditBox = stringBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(stringBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_EDIT_MODIFY,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editModifyEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }


    public void editModifyEvent(BBjEditModifyEvent ev) { // NOSONAR
        TextFieldModifyEvent dwcEv = new TextFieldModifyEvent(this.stringEditBox);
        Iterator<Consumer<TextFieldModifyEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextFieldModifyEvent> callback) {
        targets.add(callback);
    }



}
