package org.dwcj.controls.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.AbstractControl;
import org.dwcj.controls.panels.events.DivClickEvent;
import org.dwcj.controls.panels.sinks.DivClickEventSink;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * This class represents a div container, which behaves as a panel and
 * can be styled and hold other divs (panels) and controls
 */
public class Div extends AbstractDwcjPanel {

    private ArrayList<AbstractControl> controls = new ArrayList<>();
    private DivClickEventSink divClickEventSink;

    @Override
    protected void create(AbstractDwcjPanel p) {
        BBjWindow w = p.getBBjWindow();
        try {
            byte[] flags = new byte[]{(byte) 0x00, (byte) 0x10, (byte) 0x88, (byte) 0x00};
            //todo honor visible flag if set before addition to panel
            wnd = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags, Environment.getInstance().getSysGui().getAvailableContext());
            ctrl = wnd;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    @Override
    public Div add(AbstractControl ...control){
        if(this.ctrl != null){
            for(AbstractControl c: control){
                try {
                    ControlAccessor.getDefault().create(c,this);
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                }
            }
        }
        else{
            for(AbstractControl c: control){
                this.controls.add(c);
            }
        }
        return this;
    }

    /**
     * register an event callback for the click event
     *
     * @param callback A method to receive the click event
     * @return the control itself
     */
    public Div onClick(Consumer<DivClickEvent> callback) {
        if (this.divClickEventSink ==null)
            this.divClickEventSink = new DivClickEventSink(this, callback);
        else this.divClickEventSink.addCallback(callback);
        return this;
    }


    @Override
    public Div setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public Div setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public Div setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public Div setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public Div setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public Div setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public Div setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public Div addClass(String selector) {
        super.addClass(selector);
        return this;
    }

    @Override
    public Div removeClass(String selector) {
        super.removeClass(selector);
        return this;
    }

    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        while(!this.controls.isEmpty()){
            this.add(controls.remove(0));
        }

    }

}




