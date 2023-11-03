
public class Tree<T> {
    private static class Node<E> {
        E elem;
        List<Node<E>> children; // List of children
    }

    public Node (E elem, List<Node<E>> children) {
        this.elem = elem;
        this.children = children;
    }

    Node<T> root;

    public Tree() {
        root = null;
    }
    public Tree(Node<T> root) {
        this.root = root;
    }

    private static <T> int size (Node<T> node) {
        if (node == null) {
            return 0; // Árbol vacío
        }
        int size = 1;
        for (Node<T> child : node.children) {
            size += size(child);
        }
        return size;
    }
    public int size() {
        return size(root);
    }

    public <T> int height(Node<T> node) {
        if (node == null) {
            return 0;
        }
        int height = 0;
        for (Node<T> child : node.children) {
            height = Math.max(height, height(child));
        }
        return height + 1;
    }

    public int height() {
        return height(root);
    }
}