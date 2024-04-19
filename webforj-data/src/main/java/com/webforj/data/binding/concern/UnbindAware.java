package com.webforj.data.binding.concern;

import com.webforj.data.binding.BindingContext;

/**
 * Interface for beans that need to be notified when a binding is destroyed.
 *
 * @see BindingContext#unbind(Class, String)
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface UnbindAware {

  /**
   * Called when the binding to the specified bean class is destroyed.
   *
   * @param <B> the type of the bean.
   * @param context the binding context.
   * @param beanClass the class of the bean.
   * @param propertyName the name of the property.
   */
  public <B> void onUnbind(BindingContext<B> context, Class<B> beanClass, String propertyName);
}
