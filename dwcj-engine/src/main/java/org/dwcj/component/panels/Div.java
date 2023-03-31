package org.dwcj.component.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;

import org.dwcj.Environment;
import org.dwcj.annotation.AnnotationProcessor;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.panels.events.DivClickEvent;
import org.dwcj.component.panels.sinks.DivClickEventSink;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * This class represents a div container, which behaves as a panel and
 * can be styled and hold other divs (panels) and controls
 */
public class Div extends AbstractPanel {

  private ArrayList<Consumer<DivClickEvent>> callbacks = new ArrayList<>();
  private DivClickEventSink divClickEventSink;

  private final ArrayList<AbstractComponent> catchUpControls = new ArrayList<>();

  @Override
  protected void create(AbstractPanel p) {
    BBjWindow w = p.getBBjWindow();
    try {
      byte finalFlag = 0x00;
      if (Boolean.FALSE.equals(this.isVisible())) {
        finalFlag += (byte) 0x10;

      }
      if (Boolean.FALSE.equals(this.isEnabled())) {
        finalFlag += (byte) 0x20;
      }
      byte[] flags = new byte[] { (byte) 0x00, (byte) 0x10, (byte) 0x88, finalFlag };
      // todo honor visible flag if set before addition to panel
      wnd = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "",
          flags, Environment.getInstance().getSysGui().getAvailableContext());
      ctrl = wnd;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }

  }

  /**
   * Used to add controls to a panel. Multiple controls can be passed to this
   * function, and will be added in the order the arguments are passed
   * (arg0 added first, arg1 second, etc...)
   * 
   * @param control the control(s) to be added
   * @return the panel itself
   */
  @Override
  public Div add(AbstractComponent... control) {
    for (AbstractComponent c : control) {
      if (this.ctrl != null && Boolean.FALSE.equals(c.isDestroyed())) {
        try {
          AnnotationProcessor processor = new AnnotationProcessor();
          processor.processControlAnnotations(c);
          ComponentAccessor.getDefault().create(c, this);
          controls.add(c);
        } catch (IllegalAccessException e) {
          Environment.logError(e);
        }
      } else {
        if (Boolean.FALSE.equals(c.isDestroyed())) {
          this.catchUpControls.add(c);
        }
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
    if (this.ctrl != null) {
      if (this.divClickEventSink == null) {
        this.divClickEventSink = new DivClickEventSink(this, callback);
      } else {
        this.divClickEventSink.addCallback(callback);
      }
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  @Override
  public Div setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Div setVisible(Boolean visible) {
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
  public Div setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Div setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  @Override
  public Div setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public Div addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public Div removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    while (!this.catchUpControls.isEmpty()) {
      this.add(catchUpControls.remove(0));
    }

    if (!this.callbacks.isEmpty()) {
      this.divClickEventSink = new DivClickEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.divClickEventSink.addCallback(this.callbacks.remove(0));
      }
    }

  }

  /**
   * removes and destroys all controls within the Div
   */
  protected void clear() {
    for (AbstractComponent control : this.controls) {
      control.destroy();
    }
    controls.clear();

  }

}
