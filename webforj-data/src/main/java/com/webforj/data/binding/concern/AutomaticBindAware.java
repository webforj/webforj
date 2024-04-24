package com.webforj.data.binding.concern;

import com.webforj.data.binding.BindingContext;
import java.lang.reflect.Field;

/**
 * Interface for beans that need to be notified when a binding is created automatically.
 *
 * @see BindingContext#bind(Class, String)
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface AutomaticBindAware {

  /**
   * Called when the binding to the specified bean class is created automatically.
   *
   * @param <B> the type of the bean.
   * @param context the binding context.
   * @param beanClass the class of the bean
   * @param propertyName the name of the property.
   * @param field the field of the property.
   */
  public <B> void onAutomaticBind(BindingContext<B> context, Class<B> beanClass,
      String propertyName, Field field);
}
