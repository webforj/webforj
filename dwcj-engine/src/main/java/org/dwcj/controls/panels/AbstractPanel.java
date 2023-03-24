package org.dwcj.controls.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.annotations.AnnotationProcessor;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractControl;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.exceptions.DwcAnnotationException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * the base class for all panel implementations
 */
public abstract class AbstractPanel extends AbstractDwcControl {

  protected BBjWindow wnd;

  protected ArrayList<AbstractControl> controls = new ArrayList<>();
  private final HashSet<String> cssClasses = new HashSet<>();
  private final Map<String, String> styles = new HashMap<>();

  /**
   * Used to add controls to a panel. Multiple controls can be passed to this
   * function, and will be added in the order the arguments are passed
   * (arg0 added first, arg1 second, etc...)
   * 
   * @param ctrl the control(s) to be added
   * @return the panel itself
   */
  public AbstractPanel add(AbstractControl... ctrl) {
    for (AbstractControl c : ctrl) {
      if (Boolean.FALSE.equals(c.isDestroyed())) {
        try {
          AnnotationProcessor processor = new AnnotationProcessor();
          processor.processControlAnnotations(c);
          ControlAccessor.getDefault().create(c, this);
          controls.add(c);
        } catch (IllegalAccessException | DwcAnnotationException e) {
          Environment.logError(e);
        }
      }
    }
    return this;
  }

  static {
    PanelAccessor.setDefault(new PanelAccessorImpl());
  }

  /**
   * This method is only accessible through "friend" classes
   * no customer shall ever use this directly
   * 
   * @return the underlying BBjWindow
   */
  BBjWindow getBBjWindow() {
    return wnd;
  }

  /**
   * {@inheritDoc}
   */
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (!this.styles.isEmpty()) {
      for (Map.Entry<String, String> entry : this.styles.entrySet()) {
        setStyle(entry.getKey(), entry.getValue());
      }
    }

    if (!this.cssClasses.isEmpty()) {
      for (String cl : this.cssClasses) {
        addClassName(cl);
      }
    }
  }
}
