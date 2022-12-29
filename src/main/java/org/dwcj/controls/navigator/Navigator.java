package org.dwcj.controls.navigator;

import com.basis.bbj.proxies.sysgui.BBjNavigator;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.resource.RecordSet;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.navigator.events.NavigatorFirstEvent;
import org.dwcj.controls.navigator.events.NavigatorLastEvent;
import org.dwcj.controls.navigator.events.NavigatorNextEvent;
import org.dwcj.controls.navigator.events.NavigatorPreviousEvent;
import org.dwcj.controls.navigator.sinks.NavFirstEventSink;
import org.dwcj.controls.navigator.sinks.NavLastEventSink;
import org.dwcj.controls.navigator.sinks.NavNextEventSink;
import org.dwcj.controls.navigator.sinks.NavPreviousEventSink;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.HasReadOnly;

import java.util.function.Consumer;

public final class Navigator extends AbstractDwcControl implements HasReadOnly{

    private BBjNavigator bbjNavigator;

    private NavFirstEventSink navFirstEventSink;
    private NavLastEventSink navLastEventSink;
    private NavNextEventSink navNextEventSink;
    private NavPreviousEventSink navPreviousEventSink;

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addNavigator(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            catchUp();
            bbjNavigator = (BBjNavigator) ctrl;
        } catch (Exception e)  {
            Environment.logError(e);
        }
    }

    public RecordSet getTargetRecordSet() {
        try {
            return (RecordSet) bbjNavigator.getTargetRecordSet();
        } catch (BBjException e) {
            Environment.logError(e);
            return null;
        }
    }

    @Override
    public Boolean isReadOnly() {
        try {
            return bbjNavigator.isEditable();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return false;
    }

    @Override
    public Navigator setReadOnly(Boolean editable) {
        try {
            bbjNavigator.setEditable(editable);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    public Navigator onFirst(Consumer<NavigatorFirstEvent> callback) {
        if (this.navFirstEventSink==null)
            this.navFirstEventSink = new NavFirstEventSink(this, callback);
        else this.navFirstEventSink.addCallback(callback);
        return this;
    }

    public Navigator onLast(Consumer<NavigatorLastEvent> callback) {
        if (this.navLastEventSink==null)
            this.navLastEventSink = new NavLastEventSink(this, callback);
        else this.navLastEventSink.addCallback(callback);
        return this;
    }

    public Navigator onNext(Consumer<NavigatorNextEvent> callback) {
        if (this.navNextEventSink==null)
            this.navNextEventSink = new NavNextEventSink(this, callback);
        else this.navNextEventSink.addCallback(callback);
        return this;
    }

    public Navigator onPrevious(Consumer<NavigatorPreviousEvent> callback) {
        if (this.navPreviousEventSink==null)
            this.navPreviousEventSink = new NavPreviousEventSink(this, callback);
        else this.navPreviousEventSink.addCallback(callback);
        return this;
    }




    @Override
    public Navigator setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public Navigator setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public Navigator setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public Navigator setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public Navigator setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public Navigator setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public Navigator setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public Navigator addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public Navigator removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }




    public Navigator setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public Navigator setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
