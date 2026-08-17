package com.webforj.mcp;

import com.webforj.component.Component;
import com.webforj.conceiver.ConceiverProvider;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

/**
 * Describes a method that receives an MCP tool call.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class McpAppMethodDescriptor {

  private final Method invocationMethod;
  private final Class<?> ownerType;
  private final Class<? extends Component> viewType;
  private final int viewParameterIndex;
  private final int inputParameterIndex;
  private final String inputSchema;

  McpAppMethodDescriptor(Class<? extends Component> viewType, Method invocationMethod,
      Class<?> ownerType) {
    if (Modifier.isStatic(invocationMethod.getModifiers())) {
      throw new IllegalArgumentException(
          "The method " + describeMethod(invocationMethod) + " must be an instance method.");
    }
    if (!invocationMethod.trySetAccessible()) {
      throw new IllegalArgumentException(
          "The method " + describeMethod(invocationMethod) + " is not accessible.");
    }

    int resolvedViewParameter = -1;
    int resolvedInputParameter = -1;
    Class<?>[] parameterTypes = invocationMethod.getParameterTypes();

    for (int index = 0; index < parameterTypes.length; index++) {
      if (isViewParameter(parameterTypes[index], viewType)) {
        if (resolvedViewParameter >= 0) {
          throw new IllegalArgumentException("The method " + describeMethod(invocationMethod)
              + " declares more than one parameter of the view type.");
        }
        resolvedViewParameter = index;
      } else {
        if (resolvedInputParameter >= 0) {
          throw new IllegalArgumentException("The method " + describeMethod(invocationMethod)
              + " declares more than one input parameter.");
        }
        resolvedInputParameter = index;
      }
    }

    if (!ownerType.equals(viewType) && resolvedViewParameter < 0) {
      throw new IllegalArgumentException("The method " + describeMethod(invocationMethod)
          + " belongs to an actions class and must declare a parameter of the view type.");
    }

    this.invocationMethod = invocationMethod;
    this.ownerType = ownerType;
    this.viewType = viewType;
    this.viewParameterIndex = resolvedViewParameter;
    this.inputParameterIndex = resolvedInputParameter;
    this.inputSchema = resolvedInputParameter < 0 ? null
        : McpAppSchemas.generateSchemaDocument(parameterTypes[resolvedInputParameter]);
  }

  Method getInvocationMethod() {
    return invocationMethod;
  }

  Class<?> getInputType() {
    return inputParameterIndex < 0 ? null
        : invocationMethod.getParameterTypes()[inputParameterIndex];
  }

  String getInputSchema() {
    return inputSchema;
  }

  boolean hasVoidReturnType() {
    return invocationMethod.getReturnType() == Void.TYPE;
  }

  Object invoke(Component view, JsonNode arguments) {
    Object target =
        ownerType.equals(viewType) ? view : ConceiverProvider.getCurrent().get(ownerType);

    Object[] callArguments = new Object[invocationMethod.getParameterCount()];
    if (viewParameterIndex >= 0) {
      callArguments[viewParameterIndex] = view;
    }
    if (inputParameterIndex >= 0) {
      callArguments[inputParameterIndex] =
          JsonMapper.shared().convertValue(arguments, getInputType());
    }

    try {
      return invocationMethod.invoke(target, callArguments);
    } catch (InvocationTargetException e) {
      Throwable cause = e.getCause() == null ? e : e.getCause();
      if (cause instanceof RuntimeException runtime) {
        throw runtime;
      }
      throw new IllegalStateException(cause.getMessage(), cause);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException(
          "The method " + describeMethod(invocationMethod) + " is not accessible.", e);
    }
  }

  private static boolean isViewParameter(Class<?> parameterType,
      Class<? extends Component> viewType) {
    return Component.class.isAssignableFrom(parameterType)
        && parameterType.isAssignableFrom(viewType);
  }

  static String describeMethod(Method method) {
    return method.getDeclaringClass().getName() + "#" + method.getName();
  }
}
