package org.dwcj.controls.label;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.util.BBjFunctionalityHelper;

public final class Label extends AbstractDwcControl implements TextAlignable {

    private BBjStaticText bbjStaticText;


    /* top, right, bottom, left margins,
     * used simple array as user will not 
     * have to interact directly with this
     */
    // private Integer[] margins = {0,0,0,0};

    public Label() {
        this("");
    }

    /**
     * Constructor used to give the label initial text
     * @param text String value for initial display text
     */
    public Label(String text) {
        setText(text);
        this.textAlignment = Alignment.LEFT;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addStaticText(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText(), flags);
            bbjStaticText = (BBjStaticText) ctrl;
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    /**
     * Returns the value of the bottom margin added below the text
     * @return Integer value 
     */
    // public Integer getBottomMargin(){
    //     if(this.ctrl != null){
    //         try{
    //             return bbjStaticText.getBottomMargin();
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     return this.margins[2];
    // }

    // public Integer getLeftMargin(){
    //     if(this.ctrl != null){
    //         try{
    //             return bbjStaticText.getLeftMargin();
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     return this.margins[3];
    // }

    // public Integer getRightMargin(){
    //     if(this.ctrl != null){
    //         try{
    //             return bbjStaticText.getRightMargin();
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     return this.margins[1];
    // }

    // public Integer getTopMargin(){
    //     if(this.ctrl != null){
    //         try{
    //             return bbjStaticText.getTopMargin();
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     return this.margins[0];
    // }

    // public Label setBottomMargin(int margin){
    //     if(this.ctrl != null){
    //         try{
    //             bbjStaticText.setBottomMargin(margin);
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     this.margins[2] = margin;
    //     return this;
    // }
    
    // public Label setLeftMargin(int margin){
    //     if(this.ctrl != null){
    //         try{
    //             bbjStaticText.setLeftMargin(margin);
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     this.margins[3] = margin;
    //     return this;
    // }

    // public Label setRightMargin(int margin){
    //     if(this.ctrl != null){
    //         try{
    //             bbjStaticText.setRightMargin(margin);
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     this.margins[1] = margin;
    //     return this;
    // }

    // public Label setTopMargin(int margin){
    //     if(this.ctrl != null){
    //         try{
    //             bbjStaticText.setTopMargin(margin);
    //         } catch (BBjException e){
    //             Environment.logError(e);
    //         }
    //     }
    //     this.margins[0] = margin;
    //     return this;
    // }



    @Override
    public Alignment getTextAlignment(){
        return this.textAlignment;
    }

    @Override
    public Label setTextAlignment(Alignment alignment){
        if(this.ctrl != null){
            try{
                ((BBjStaticText) ctrl).setAlignment(alignment.textPosition);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.textAlignment = alignment;
        return this;
    }



    @Override
    public Label setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public Label setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public Label setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public Label setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public Label setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public Label setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public Label setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public Label addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public Label removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();
    }



}
