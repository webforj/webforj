package com.webforj.data.binding.concern;

import com.webforj.data.binding.BindingContext;

/**
 * An interface for beans that need to be notified when a binding is created.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface BindAware {

  /**
   * Called when the binding to the specified bean class is created.
   *
   * @param <B> the type of the bean.
   * @param context the binding context.
   * @param beanClass the class of the bean.
   * @param propertyName the name of the property.
   */
  public <B> void onBind(BindingContext<B> context, Class<B> beanClass, String propertyName);
}
