package org.dwcj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import java.util.ArrayList;
import java.util.function.Consumer;
import org.dwcj.Environment;
import org.dwcj.annotation.AnnotationProcessor;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.window.event.WindowClickEvent;
import org.dwcj.component.window.sink.WindowClickEventSink;


/**
 * This class represents a div container, which behaves as a panel and can be styled and hold other
 * divs (panels) and controls.
 */
public class Panel extends AbstractWindow implements HasEnable{

  private ArrayList<Consumer<WindowClickEvent>> callbacks = new ArrayList<>();
  private WindowClickEventSink divClickEventSink;

  private final ArrayList<AbstractComponent> catchUpControls = new ArrayList<>();

  @Override
  protected void create(AbstractWindow p) {
    BBjWindow w = p.getBBjWindow();
    try {
      byte finalFlag = 0x00;
      if (Boolean.FALSE.equals(this.isVisible())) {
        finalFlag += (byte) 0x10;

      }
      if (Boolean.FALSE.equals(this.isEnabled())) {
        finalFlag += (byte) 0x20;
      }
      byte[] flags = new byte[] {(byte) 0x00, (byte) 0x10, (byte) 0x88, finalFlag};
      // todo honor visible flag if set before addition to panel
      wnd = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, "", flags, Environment.getInstance().getSysGui().getAvailableContext());
      ctrl = wnd;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }

  }

  /**
   * Used to add controls to a panel. Multiple controls can be passed to this function, and will be
   * added in the order the arguments are passed (arg0 added first, arg1 second, etc...)
   *
   * @param control the control(s) to be added
   * @return the panel itself
   */
  @Override
  public Panel add(AbstractComponent... control) {
    for (AbstractComponent c : control) {
      if (this.ctrl != null && Boolean.FALSE.equals(c.isDestroyed())) {
        try {
          AnnotationProcessor processor = new AnnotationProcessor();
          processor.processControlAnnotations(c);
          ComponentAccessor.getDefault().create(c, this);
          components.put(c.getComponentId(), c);
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
   * register an event callback for the click event.
   *
   * @param callback A method to receive the click event
   * @return the control itself
   */
  public Panel onClick(Consumer<WindowClickEvent> callback) {
    if (this.ctrl != null) {
      if (this.divClickEventSink == null) {
        this.divClickEventSink = new WindowClickEventSink(this, callback);
      } else {
        this.divClickEventSink.addCallback(callback);
      }
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  @Override
  public Panel setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Panel setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public Panel setEnabled(Boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override 
  public Boolean isEnabled(){
    return super.isComponentEnabled();
  }

  @Override
  public Panel setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public Panel setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Panel setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public Panel addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public Panel removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    while (!this.catchUpControls.isEmpty()) {
      this.add(catchUpControls.remove(0));
    }

    if (!this.callbacks.isEmpty()) {
      this.divClickEventSink = new WindowClickEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.divClickEventSink.addCallback(this.callbacks.remove(0));
      }
    }

  }

  /**
   * removes and destroys all controls within the Div.
   */
  protected void clear() {
    for (AbstractComponent component : this.components.values()) {
      component.destroy();
    }
    components.clear();

  }

}
