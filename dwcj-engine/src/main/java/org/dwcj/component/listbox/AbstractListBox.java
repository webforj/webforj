package org.dwcj.component.listbox;

import java.util.HashMap;
import java.util.Map;

import org.dwcj.component.AbstractDwcComponent;

import com.basis.startup.type.BBjVector;


public abstract class AbstractListBox extends AbstractDwcComponent {

  protected BBjVector data2 = new BBjVector();
  protected Map<Object, String> values = new HashMap<>();

  protected abstract void populate();

}
