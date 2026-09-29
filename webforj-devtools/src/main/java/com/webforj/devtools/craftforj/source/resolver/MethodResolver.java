package com.webforj.devtools.craftforj.source.resolver;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SuperExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithArguments;
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

/**
 * Binds the calls and the creations written in a source file to the methods and the constructors
 * the application has loaded.
 *
 * <p>
 * A call is bound the way the compiler picks an overload, by the number of arguments and by the
 * types of the arguments that can be resolved. An argument whose type stays unknown fits every
 * parameter, so a call can bind to more than one overload.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class MethodResolver {

  private MethodResolver() {}

  /**
   * Finds the methods a call can be bound to.
   *
   * @param call the call
   *
   * @return the methods that accept the arguments, empty when the receiver cannot be resolved
   */
  public static List<Method> findMethods(MethodCallExpr call) {
    Class<?> receiver = resolveReceiver(call);
    if (receiver == null) {
      return List.of();
    }

    Map<String, Method> candidates = new LinkedHashMap<>();
    try {
      for (Class<?> current = receiver; current != null; current = current.getSuperclass()) {
        collect(current.getDeclaredMethods(), call.getNameAsString(), candidates);
      }

      collect(receiver.getMethods(), call.getNameAsString(), candidates);
    } catch (LinkageError | SecurityException e) {
      return List.of();
    }

    return pickApplicable(new ArrayList<>(candidates.values()), call.getArguments());
  }

  /**
   * Resolves the class a call is made on.
   *
   * @param call the call
   *
   * @return the class of the receiver, the enclosing class for a call without a receiver, or
   *         {@code null} when it cannot be resolved
   */
  public static Class<?> resolveReceiver(MethodCallExpr call) {
    Expression scope = call.getScope().orElse(null);
    if (scope == null) {
      return TypeResolver.resolveEnclosing(call);
    }

    return TypeResolver.resolveType(scope);
  }

  /**
   * Resolves the type a call returns.
   *
   * @param call the call
   *
   * @return the return type every method the call can be bound to agrees on, or {@code null}
   */
  public static Class<?> resolveReturnType(MethodCallExpr call) {
    Class<?> receiver = resolveReceiver(call);
    List<Class<?>> types = new ArrayList<>();
    for (Method method : findMethods(call)) {
      Class<?> type = resolveFluentType(method, receiver, call);
      if (!types.contains(type)) {
        types.add(type);
      }
    }

    return types.size() == 1 ? types.get(0) : null;
  }

  /**
   * Finds the end of the chain of calls made on an expression.
   *
   * @param expression the expression the chain starts at
   *
   * @return the last call of the chain that still returns a component, or the expression itself
   *         when no such call is made on it
   */
  public static Expression findChainEnd(Expression expression) {
    Expression current = expression;
    while (current.getParentNode().orElse(null) instanceof MethodCallExpr call
        && call.getScope().orElse(null) == current) {
      Class<?> returned = resolveReturnType(call);
      if (returned != null && !TypeResolver.isComponent(returned)) {
        break;
      }

      current = call;
    }

    return current;
  }

  /**
   * Checks whether a class hands an argument to a parameter that takes any number of components.
   *
   * @param type the class that declares the method
   * @param method the method name, or {@code null} for the constructors of the class
   * @param index the position of the argument
   *
   * @return {@code true} when a method of that name, or a constructor, takes the argument in a
   *         variable arity parameter of a component type
   */
  public static boolean isComponentVarargs(Class<?> type, String method, int index) {
    if (type == null) {
      return false;
    }

    try {
      List<Executable> candidates = new ArrayList<>();
      if (method == null) {
        candidates.addAll(List.of(type.getConstructors()));
      } else {
        Arrays.stream(type.getMethods()).filter(candidate -> candidate.getName().equals(method))
            .forEach(candidates::add);
      }

      return candidates.stream().anyMatch(candidate -> takesComponents(candidate, index));
    } catch (LinkageError | SecurityException e) {
      return false;
    }
  }

  /**
   * Checks whether an argument is handed to a parameter that takes any number of components.
   *
   * @param receiver the call or the creation that takes the argument
   * @param index the position of the argument
   *
   * @return {@code true} when every method or constructor the receiver can be bound to takes the
   *         argument in a variable arity parameter of a component type
   */
  public static boolean isComponentVarargs(NodeWithArguments<?> receiver, int index) {
    List<? extends Executable> bound = find(receiver);

    return !bound.isEmpty()
        && bound.stream().allMatch(executable -> takesComponents(executable, index));
  }

  /**
   * Checks whether the methods a call can be bound to are declared by the project itself.
   *
   * @param call the call
   * @param project tells whether a class has its source in the project
   *
   * @return {@code true} when the source of the enclosing class declares the method, or a class of
   *         the project declares one of the methods the call can be bound to
   */
  public static boolean isProjectMethod(MethodCallExpr call, Predicate<Class<?>> project) {
    return isDeclaredBySource(call)
        || findMethods(call).stream().map(Method::getDeclaringClass).anyMatch(project);
  }

  /**
   * Checks whether every method a call can be bound to is static.
   *
   * @param call the call
   *
   * @return {@code true} when the call is bound and made on a class instead of an object
   */
  public static boolean isStatic(MethodCallExpr call) {
    List<Method> methods = findMethods(call);

    return !methods.isEmpty()
        && methods.stream().allMatch(method -> Modifier.isStatic(method.getModifiers()));
  }

  /**
   * Checks whether a call or a creation can be written without one of its arguments.
   *
   * @param receiver the call or the creation that takes the argument
   * @param index the position of the argument
   *
   * @return {@code true} when the receiver binds to one method or constructor and the same class
   *         declares another one that takes the remaining parameters and returns the same type
   */
  public static boolean hasOverloadWithout(NodeWithArguments<?> receiver, int index) {
    List<? extends Executable> bound = find(receiver);
    if (bound.size() != 1 || bound.get(0).isVarArgs()) {
      return false;
    }

    Executable original = bound.get(0);
    List<Class<?>> remaining = new ArrayList<>(Arrays.asList(original.getParameterTypes()));
    if (index >= remaining.size()) {
      return false;
    }

    remaining.remove(index);
    Class<?> owner = original.getDeclaringClass();
    Executable[] siblings =
        original instanceof Method ? owner.getDeclaredMethods() : owner.getDeclaredConstructors();

    return Arrays.stream(siblings)
        .anyMatch(sibling -> sibling.getName().equals(original.getName()) && !sibling.isVarArgs()
            && Arrays.asList(sibling.getParameterTypes()).equals(remaining)
            && getProducedType(sibling).equals(getProducedType(original)));
  }

  /**
   * Finds the constructors a creation can be bound to.
   *
   * @param creation the creation
   *
   * @return the constructors that accept the arguments, empty when the type cannot be resolved
   */
  private static List<Executable> findConstructors(ObjectCreationExpr creation) {
    Class<?> type = TypeResolver.resolve(creation.getType(), creation);
    if (type == null) {
      return List.of();
    }

    try {
      return pickApplicable(List.of(type.getDeclaredConstructors()), creation.getArguments());
    } catch (LinkageError | SecurityException e) {
      return List.of();
    }
  }

  /**
   * Finds the constructors a constructor can hand over to.
   *
   * @param invocation the call of {@code this(...)} or {@code super(...)}
   *
   * @return the constructors that accept the arguments, empty when the class cannot be resolved
   */
  private static List<Executable> findConstructors(ExplicitConstructorInvocationStmt invocation) {
    Class<?> type = invocation.isThis() ? TypeResolver.resolveEnclosing(invocation)
        : TypeResolver.resolveParent(invocation);
    if (type == null) {
      return List.of();
    }

    try {
      return pickApplicable(List.of(type.getDeclaredConstructors()), invocation.getArguments());
    } catch (LinkageError | SecurityException e) {
      return List.of();
    }
  }

  /**
   * Checks whether the source of the enclosing class declares the method a call is made on.
   *
   * @param call the call
   *
   * @return {@code true} when the call has no receiver, or {@code this} as its receiver, and the
   *         class it is written in declares a method of that name and number of parameters
   */
  private static boolean isDeclaredBySource(MethodCallExpr call) {
    Expression scope = call.getScope().orElse(null);
    if (scope != null && !(scope instanceof ThisExpr) && !(scope instanceof SuperExpr)) {
      return false;
    }

    Node type = VariableResolver.findEnclosingType(call);
    while (type != null) {
      boolean declares = type.getChildNodes().stream()
          .anyMatch(member -> member instanceof CallableDeclaration<?> callable
              && callable.getNameAsString().equals(call.getNameAsString())
              && callable.getParameters().size() == call.getArguments().size());
      if (declares) {
        return true;
      }

      type = scope == null ? VariableResolver.findEnclosingType(type) : null;
    }

    return false;
  }

  private static List<? extends Executable> find(NodeWithArguments<?> receiver) {
    if (receiver instanceof MethodCallExpr call) {
      return findMethods(call);
    }

    if (receiver instanceof ExplicitConstructorInvocationStmt invocation) {
      return findConstructors(invocation);
    }

    return receiver instanceof ObjectCreationExpr creation ? findConstructors(creation) : List.of();
  }

  private static boolean takesComponents(Executable executable, int index) {
    Class<?>[] parameters = executable.getParameterTypes();

    return executable.isVarArgs() && index >= parameters.length - 1
        && TypeResolver.isComponent(parameters[parameters.length - 1].getComponentType());
  }

  private static Class<?> getProducedType(Executable executable) {
    return executable instanceof Method method ? method.getReturnType()
        : executable.getDeclaringClass();
  }

  // A method of a parent class that a child class overrides is collected once, from the child
  private static void collect(Method[] methods, String name, Map<String, Method> candidates) {
    for (Method method : methods) {
      if (method.getName().equals(name) && !method.isBridge() && !method.isSynthetic()) {
        candidates.putIfAbsent(Arrays.toString(method.getParameterTypes()), method);
      }
    }
  }

  private static <T extends Executable> List<T> pickApplicable(List<T> candidates,
      List<Expression> arguments) {
    List<Class<?>> types = arguments.stream().map(TypeResolver::resolveType).collect(ArrayList::new,
        ArrayList::add, ArrayList::addAll);
    List<T> fixed = candidates.stream()
        .filter(candidate -> isApplicable(candidate.getParameterTypes(), types)).toList();
    if (!fixed.isEmpty()) {
      return pickMostSpecific(fixed);
    }

    // The compiler turns to variable arity only when no method takes the arguments as written
    return candidates.stream().filter(Executable::isVarArgs)
        .filter(
            candidate -> isApplicable(spread(candidate.getParameterTypes(), types.size()), types))
        .toList();
  }

  // A method is left out when another one takes the same arguments in narrower parameters
  private static <T extends Executable> List<T> pickMostSpecific(List<T> applicable) {
    return applicable.stream().filter(candidate -> applicable.stream()
        .noneMatch(other -> other != candidate && isNarrower(other, candidate))).toList();
  }

  private static boolean isNarrower(Executable first, Executable second) {
    Class<?>[] narrow = first.getParameterTypes();
    Class<?>[] wide = second.getParameterTypes();
    boolean same = true;
    for (int index = 0; index < narrow.length; index++) {
      if (!isAssignable(wide[index], narrow[index])) {
        return false;
      }

      same &= narrow[index].equals(wide[index]);
    }

    return !same;
  }

  private static Class<?>[] spread(Class<?>[] parameters, int size) {
    int fixed = parameters.length - 1;
    if (size < fixed) {
      return parameters;
    }

    Class<?>[] spread = Arrays.copyOf(parameters, size);
    Arrays.fill(spread, fixed, size, parameters[fixed].getComponentType());

    return spread;
  }

  private static boolean isApplicable(Class<?>[] parameters, List<Class<?>> arguments) {
    if (parameters.length != arguments.size()) {
      return false;
    }

    for (int index = 0; index < parameters.length; index++) {
      Class<?> argument = arguments.get(index);
      if (argument != null && !isAssignable(parameters[index], argument)) {
        return false;
      }
    }

    return true;
  }

  private static boolean isAssignable(Class<?> parameter, Class<?> argument) {
    if (parameter.isAssignableFrom(argument)) {
      return true;
    }

    Class<?> boxedParameter = box(parameter);
    Class<?> boxedArgument = box(argument);
    if (boxedParameter.isAssignableFrom(boxedArgument)) {
      return true;
    }

    return argument.isPrimitive() && parameter.isPrimitive() && isWider(parameter, argument);
  }

  private static boolean isWider(Class<?> parameter, Class<?> argument) {
    List<Class<?>> order =
        List.of(byte.class, short.class, int.class, long.class, float.class, double.class);
    int from = argument == char.class ? order.indexOf(int.class) - 1 : order.indexOf(argument);
    int to = order.indexOf(parameter);

    return from >= 0 && to > from || argument == char.class && to >= order.indexOf(int.class);
  }

  private static Class<?> box(Class<?> type) {
    if (!type.isPrimitive()) {
      return type;
    }

    if (type == int.class) {
      return Integer.class;
    }

    if (type == boolean.class) {
      return Boolean.class;
    }

    if (type == long.class) {
      return Long.class;
    }

    if (type == double.class) {
      return Double.class;
    }

    if (type == float.class) {
      return Float.class;
    }

    if (type == char.class) {
      return Character.class;
    }

    if (type == short.class) {
      return Short.class;
    }

    return type == byte.class ? Byte.class : Void.class;
  }

  // A fluent method declared as "T setText(String)" returns the class that fills in T
  private static Class<?> resolveFluentType(Method method, Class<?> receiver, MethodCallExpr call) {
    if (Modifier.isStatic(method.getModifiers())
        || !(method.getGenericReturnType() instanceof TypeVariable<?> variable)
        || !(variable.getGenericDeclaration() instanceof Class<?>)) {
      return method.getReturnType();
    }

    Type bound = receiver == null ? null : resolveVariable(receiver, Map.of(), variable);
    Class<?> raw = getRawType(bound == null ? resolveWrittenVariable(call, variable) : bound);

    return raw != null ? raw : method.getReturnType();
  }

  private static Type resolveWrittenVariable(MethodCallExpr call, TypeVariable<?> variable) {
    for (ClassOrInterfaceType written : findWrittenTypes(call)) {
      Class<?> raw = TypeResolver.resolve(written, call);
      var arguments = written.getTypeArguments().orElse(null);
      if (raw == null || arguments == null || arguments.size() != raw.getTypeParameters().length) {
        continue;
      }

      Map<TypeVariable<?>, Type> own = new HashMap<>();
      for (int index = 0; index < arguments.size(); index++) {
        own.put(raw.getTypeParameters()[index], TypeResolver.resolve(arguments.get(index), call));
      }

      Type resolved = resolveVariable(raw, own, variable);
      if (resolved != null) {
        return resolved;
      }
    }

    return null;
  }

  // The source names what fills in the variables of a type where the receiver is declared,
  // "Supplier<Button> supplier", and where a class names its parent,
  // "View extends Composite<FlexLayout>"
  private static List<ClassOrInterfaceType> findWrittenTypes(MethodCallExpr call) {
    Expression scope = call.getScope().orElse(null);
    if (scope == null || scope instanceof ThisExpr || scope instanceof SuperExpr) {
      return VariableResolver.findEnclosingType(call) instanceof ClassOrInterfaceDeclaration type
          ? type.getExtendedTypes()
          : List.of();
    }

    if (!(scope instanceof NameExpr) && !(scope instanceof FieldAccessExpr)) {
      return List.of();
    }

    Node declaration = VariableResolver.findDeclaration(scope);
    if (declaration instanceof VariableDeclarator variable
        && variable.getType() instanceof ClassOrInterfaceType named) {
      return List.of(named);
    }

    return declaration instanceof Parameter parameter
        && parameter.getType() instanceof ClassOrInterfaceType named ? List.of(named) : List.of();
  }

  private static Type resolveVariable(Type type, Map<TypeVariable<?>, Type> bound,
      TypeVariable<?> variable) {
    Class<?> raw = getRawType(type);
    if (raw == null) {
      return null;
    }

    // What the type variables of this class stand for, as seen from the class the walk began at
    Map<TypeVariable<?>, Type> own = new HashMap<>();
    if (type instanceof ParameterizedType parameterized) {
      Type[] arguments = parameterized.getActualTypeArguments();
      for (int index = 0; index < arguments.length; index++) {
        Type argument = arguments[index];
        own.put(raw.getTypeParameters()[index],
            argument instanceof TypeVariable<?> ? bound.get(argument) : argument);
      }
    }

    return resolveVariable(raw, own, variable);
  }

  private static Type resolveVariable(Class<?> raw, Map<TypeVariable<?>, Type> own,
      TypeVariable<?> variable) {
    if (raw.equals(variable.getGenericDeclaration())) {
      return own.get(variable);
    }

    List<Type> parents = new ArrayList<>();
    if (raw.getGenericSuperclass() != null) {
      parents.add(raw.getGenericSuperclass());
    }

    parents.addAll(Arrays.asList(raw.getGenericInterfaces()));
    for (Type parent : parents) {
      Type resolved = resolveVariable(parent, own, variable);
      if (resolved != null) {
        return resolved;
      }
    }

    return null;
  }

  private static Class<?> getRawType(Type type) {
    if (type instanceof Class<?> plain) {
      return plain;
    }

    return type instanceof ParameterizedType parameterized
        && parameterized.getRawType() instanceof Class<?> raw ? raw : null;
  }
}
