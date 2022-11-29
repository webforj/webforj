package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.SliderOnControlScrollEvent;
import org.dwcj.events.sinks.SliderOnControlScrollEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public final class Slider extends AbstractDwcControl implements IFocusable, IMouseWheelEnableable, ITabTraversable {

    private BBjSlider bbjSlider;

    
    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, SUCCESS, WARNING
    }
    
    private ArrayList<Consumer<SliderOnControlScrollEvent>> callbacks = new ArrayList<>();
    private SliderOnControlScrollEventSink scrollEventSink;
    
    private Boolean horizontal = true;
    private Boolean inverted = false;
    private Integer majorTickSpacing = 1;
    private Integer minorTickSpacing = 1;
    private Integer maximum = 100;
    private Integer minimum = 0;
    private Boolean paintLabels = false;
    private Boolean paintTicks = false;
    private Boolean snapToTicks = false;
    private Integer value = 0;     

    
    public Slider(){
        this(true);
    }

    public Slider(Boolean horizontal){ 
        this.horizontal = horizontal; 
        this.focusable = true;
        this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
        this.tabTraversable = true;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte bFlag = (byte)0x00;

            if(!this.isEnabled()){
                bFlag += (byte)0x01;
            }
            if(!this.isVisible()){
                bFlag += (byte)0x10;
            }

            byte[] flags = new byte[]{(byte)0x00, bFlag};

            if (horizontal)
                ctrl = w.addHorizontalSlider(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, flags);
            else
                ctrl = w.addVerticalSlider(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, flags);
                bbjSlider = (BBjSlider) ctrl;
                catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public Slider onScroll(Consumer<SliderOnControlScrollEvent> callback) {
        if(this.ctrl != null){
            if(this.scrollEventSink == null){
                this.scrollEventSink = new SliderOnControlScrollEventSink(this);
            }
            this.scrollEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }

    /*
     * ==I tested the set method and no inversion happens, but this method does properly return the Boolean value
     * if it's been changed== -MH
     */

    /**
     * This method gets the orientation of the ProgressBar control. By default, the minimum value of a vertical slider is at the bottom and the maximum value is at the top. For a horizontal slider, the minimum value is to the left and the maximum value is to the right. The orientation reverses for inverted sliders.
     * @return Returns whether the control orientation is inverted.
     */
    public Boolean isInverted() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getInverted();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.inverted;
    }

    /**
     * This method returns the labels from a ProgressBar control.
     * @return Returns a Java Map<Integer,String> structure, where each Integer key is the slider position of the corresponding String label.
     */
    public Map<Integer,String> getLabels() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getLabels();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return new HashMap<>();
    }

    /**
     * This method queries the slider's major tick spacing.
     * @return Returns the slider's major tick spacing.
     */
    public Integer getMajorTickSpacing() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getMajorTickSpacing();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.majorTickSpacing;
    }

    /**
     * This method returns the maximum value of the ProgressBar control.
     * @return Returns the maximum value of the control.
     */
    public Integer getMaximum() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getMaximum();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.maximum;
    }

    /**
     * This method returns the minimum value of the ProgressBar control.
     * @return Returns the minimum value of the control.
     */
    public Integer getMinimum() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getMinimum();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.minimum;
    }

    /**
     * This method queries the minor tick spacing of the ProgressBar control.
     * @return Returns the slider's minor tick spacing.
     */
    public Integer getMinorTickSpacing() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getMinorTickSpacing();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.minorTickSpacing;
    }

    /**
     * This method returns the orientation of the ProgressBar control.
     * @return Returns the orientation of the control (false = HORIZONTAL, true = VERTICAL).
     */
    public Integer getOrientation() {
        if(this.ctrl != null){
            return bbjSlider.getOrientation();
        }
        if(this.horizontal){
            return 0;
        }
        return 1;
    }

    /**
     * This method queries whether to paint labels on the ProgressBar control.
     * @return Returns whether labels are painted on this slider.
     */
    public Boolean isPaintLabels() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getPaintLabels();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.paintLabels;
    }

    /**
     * This method queries whether to paint ticks on the ProgressBar control.
     * @return Returns whether ticks are painted on this slider.
     */
    public Boolean isPaintTicks() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getPaintTicks();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.paintTicks;
    }

    /**
     * This method queries whether a ProgressBar control should snap to the nearest tick when the user drags the thumb.
     * @return Returns whether the BBjSlider should snap to the nearest tick when the user drags the thumb.
     */
    public Boolean isSnapToTicks() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getSnapToTicks();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.snapToTicks;
    }

    /**
     * This method returns the current value of the ProgressBar control.
     * @return Returns the current value of the control.
     */
    public Integer getValue() {
        if(this.ctrl != null){
            try {
                return bbjSlider.getValue();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.value;
    }

    /**
     * This method sets the orientation of the ProgressBar control. By default, the minimum value of a vertical slider is at the bottom and the maximum value is at the top. For a horizontal slider, the minimum value is to the left and the maximum value is to the right. The orientation reverses for inverted sliders.
     * @param inverted - Specifies whether the slider orientation is inverted.
     * @return Returns this
     */
    public Slider setInverted(Boolean inverted) {
        if(this.ctrl != null){
            try {
                bbjSlider.setInverted(inverted);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.inverted = inverted;
        return this;
    }

    /**
     * This method sets the custom labels for a ProgressBar control.
     * @param labels - A Java Map<Integer,String> structure, where the Integer key is the slider position of the corresponding String label.
     * @return Returns this
     */
    public Slider setLabels(Map<Integer,String> labels) {
        if(this.ctrl != null){
            try {
                bbjSlider.setLabels(labels);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    /**
     * This method sets the major tick spacing for a ProgressBar control.
     * @param prop - Specifies the major tick spacing.
     * @return Returns this
     */
    public Slider setMajorTickSpacing(Integer tick) {
        if(this.ctrl != null){
            try {
                bbjSlider.setMajorTickSpacing(tick);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.majorTickSpacing = tick;
        return this;
    }

    /**
     * This method sets the maximum value of the ProgressBar control.
     * @param value - Specifies the maximum value.
     * @return Returns this
     */
    public Slider setMaximum(Integer maximum) {
        if(this.ctrl != null){
            try {
                bbjSlider.setMaximum(maximum);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.maximum = maximum;
        return this;
    }

    /**
     * This method sets the minimum value of the ProgressBar control.
     * @param value - Specifies the minimum value.
     * @return Returns this
     */
    public Slider setMinimum(Integer minimum) {
        if(this.ctrl != null){
            try {
                bbjSlider.setMinimum(minimum);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.minimum = minimum;
        return this;
    }

    /**
     * This method sets the minor tick spacing of a ProgressBar control.
     * @param tick - Specifies the minor tick spacing.
     * @return Returns this
     */
    public Slider setMinorTickSpacing(Integer tick) {
        if(this.ctrl != null){
            try {
                bbjSlider.setMinorTickSpacing(tick);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.minorTickSpacing = tick;
        return this;
    }

    /**
     * This method sets whether labels are painted on a ProgressBar control.
     * @param paint - Specifies whether labels are painted on the
     * @return Returns this
     */
    public Slider setPaintLabels(Boolean paint) {
        if(this.ctrl != null){
            try {
                bbjSlider.setPaintLabels(paint);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.paintLabels = paint;
        return this;
    }

    /**
     * This method sets whether ticks are painted on a ProgressBar control.
     * @param paint - Specifies whether ticks are painted on the control.
     * @return Returns this
     */
    public Slider setPaintTicks(Boolean paint) {
        if(this.ctrl != null){
            try {
                bbjSlider.setPaintTicks(paint);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.paintTicks = paint;
        return this;
    }

    /**
     * This method sets whether a ProgressBar control should snap to the nearest tick when the user drags the thumb.
     * @param snap - Specifies whether the control should snap to the nearest tick when the user drags the thumb.
     * @return Returns this
     */
    public Slider setSnapToTicks(Boolean snap) {
        if(this.ctrl != null){
            try {
                bbjSlider.setSnapToTicks(snap);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.snapToTicks = snap;
        return this;
    }

    /**
     * This method sets the value of the ProgressBar control.
     * @param value - Specifies the slider value.
     * @return Returns this
     */
    public Slider setValue(Integer value) {
        if(this.ctrl != null){
            try {
                bbjSlider.setValue(value);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.value = value;
        return this;
    }



    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjSlider.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.focusable;
    }

    @Override
    public Slider setFocusable(Boolean focusable){
        if(this.ctrl != null) {
            try{
                bbjSlider.setFocusable(focusable);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.focusable = focusable;
        return this;
    }

    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                bbjSlider.isTabTraversable();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.tabTraversable;
    }

    @Override
    public Slider setTabTraversable(Boolean traverse){
        if(this.ctrl != null){
            try{
                bbjSlider.setTabTraversable(traverse);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.tabTraversable = traverse;
        return this;
    }


    @Override
    public MouseWheelCondition getScrollWheelBehavior(){
        return this.mouseWheelCondition;
    }

    @Override
    public Slider setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                bbjSlider.setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }


    public Slider setText(String text) {
        super.setControlText(text);
        return this;
    }

    public Slider setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public Slider setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public Slider setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public Slider setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public Slider setID(String id){
        super.setControlID(id);
        return this;
    }

    public Slider setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public Slider addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public Slider removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }




    public Slider setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }





    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();


        if(!this.callbacks.isEmpty()){
            this.scrollEventSink = new SliderOnControlScrollEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.scrollEventSink.addCallback(this.callbacks.remove(0));
            }
        }


        if(this.inverted != false){
            this.setInverted(this.inverted);
        }

        if(this.majorTickSpacing != 1){
            this.setMajorTickSpacing(this.majorTickSpacing);
        }

        if(this.minorTickSpacing != 0){
            this.setMinorTickSpacing(this.minorTickSpacing);
        }

        if(this.maximum != 100){
            this.setMaximum(this.maximum);
        }

        if(this.minimum != 0){
            this.setMinimum(this.minimum);
        }

        if(this.paintLabels != false){
            this.setPaintLabels(this.paintLabels);
        }

        if(this.paintTicks != false){
            this.setPaintTicks(this.paintLabels);
        }

        if(this.snapToTicks != false){
            this.setPaintTicks(this.paintLabels);
        }

        if(this.value != 0){
            this.setValue(this.value);
        }



        if(this.focusable != true){
            this.setFocusable(this.focusable);
        }

        if(this.tabTraversable != true){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.mouseWheelCondition != MouseWheelCondition.DEFAULT){
            this.setScrollWheelBehavior(this.mouseWheelCondition);
        }


    }

}
