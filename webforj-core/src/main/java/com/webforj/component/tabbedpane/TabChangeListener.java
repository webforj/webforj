package com.webforj.component.tabbedpane;

import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.startup.type.BBjException;
import com.webforj.exceptions.WebforjRuntimeException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * An Tab change listener. When a property of a Tab changes, this listener will update the tabbed
 * pane in the client.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
final class TabChangeListener implements PropertyChangeListener {
  private final TabbedPane tabbedPane;

  /**
   * Constructs a new TabChangeListener.
   *
   * @param tabbedPane The tabbed pane instance.
   */
  public TabChangeListener(TabbedPane tabbedPane) {
    this.tabbedPane = tabbedPane;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    Tab tab = (Tab) evt.getSource();
    String property = evt.getPropertyName();
    int index = this.tabbedPane.indexOf(tab);

    if (index >= 0 && this.tabbedPane.isAttached()) {
      switch (property) {
        case "text":
          setTextAt(index, (String) evt.getNewValue());
          break;

        case "enabled":
          setEnabledAt(index, (boolean) evt.getNewValue());
          break;

        case "closable":
          setCloseableAt(index, (boolean) evt.getNewValue());
          break;

        case "tooltip":
          setToolTip(index, (String) evt.getNewValue());
          break;

        default:
          break;
      }
    }
  }

  /**
   * Sets the text of the tab at the specified index.
   *
   * @param index The index of the tab.
   * @param text The text to set.
   */
  private void setTextAt(int index, String text) {
    BBjTabCtrl tabCtrl = this.tabbedPane.inferTabCtrl();
    if (tabCtrl != null) {
      try {
        tabCtrl.setTitleAt(index, text);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
  }

  /**
   * Sets the enabled state of the tab at the specified index.
   *
   * @param index The index of the tab.
   * @param enabled The enabled state to set.
   */
  private void setEnabledAt(int index, boolean enabled) {
    BBjTabCtrl tabCtrl = this.tabbedPane.inferTabCtrl();
    if (tabCtrl != null) {
      try {
        tabCtrl.setEnabledAt(index, enabled);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
  }

  /**
   * Sets the closable state of the tab at the specified index.
   *
   * @param index The index of the tab.
   * @param closable The closable state to set.
   */
  private void setCloseableAt(int index, boolean closable) {
    BBjTabCtrl tabCtrl = this.tabbedPane.inferTabCtrl();
    if (tabCtrl != null) {
      try {
        tabCtrl.setCloseableAt(index, closable);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
  }

  /**
   * Sets the tooltip of the tab at the specified index.
   *
   * @param index The index of the tab.
   * @param tooltip The tooltip to set.
   */
  private void setToolTip(int index, String tooltip) {
    BBjTabCtrl tabCtrl = this.tabbedPane.inferTabCtrl();
    if (tabCtrl != null) {
      try {
        tabCtrl.setToolTipTextAt(index, tooltip);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
  }
}
