package org.autorefactor.cfg;

import org.eclipse.jdt.core.dom.Expression;

public class CFGEdgeBuilder {

    private Expression condition;
    /** TODO JNR rename. */
    private boolean evaluationResult;
    private CFGBasicBlock sourceBlock;
    private CFGBasicBlock targetBlock;
    /** Marks a "jumping" edge: and edge built because of an exception escaping a try statement. */
    private boolean jumping;
    /** Prevents building twice. */
    private CFGEdge built;

    public CFGEdgeBuilder(CFGBasicBlock sourceBlock) {
        this(null, false, sourceBlock);
    }

    public CFGEdgeBuilder(Expression condition, boolean evaluationResult,
            CFGBasicBlock sourceBlock) {
        this.condition = condition;
        this.evaluationResult = evaluationResult;
        this.sourceBlock = sourceBlock;
    }

    public CFGEdgeBuilder(CFGBasicBlock throwingBlock, boolean jumping) {
        this.sourceBlock = throwingBlock;
        this.jumping = jumping;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((condition == null) ? 0 : condition.hashCode());
        result = prime * result + (evaluationResult ? 1231 : 1237);
        result = prime * result
                + ((sourceBlock == null) ? 0 : sourceBlock.hashCode());
        result = prime * result
                + ((targetBlock == null) ? 0 : targetBlock.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        CFGEdgeBuilder other = (CFGEdgeBuilder) obj;
        if (condition == null) {
            if (other.condition != null)
                return false;
        } else if (!condition.equals(other.condition))
            return false;
        if (evaluationResult != other.evaluationResult)
            return false;
        if (sourceBlock == null) {
            if (other.sourceBlock != null)
                return false;
        } else if (!sourceBlock.equals(other.sourceBlock))
            return false;
        if (targetBlock == null) {
            if (other.targetBlock != null)
                return false;
        } else if (!targetBlock.equals(other.targetBlock))
            return false;
        return true;
    }

    public CFGEdgeBuilder withTarget(CFGBasicBlock target) {
        this.targetBlock = target;
        return this;
    }

    public CFGEdge build() {
        if (sourceBlock == null) {
            throw new IllegalStateException("sourceBlock is mandatory");
        }
        if (targetBlock == null) {
            throw new IllegalStateException("targetBlock is mandatory");
        }
        if (built != null) {
            throw new IllegalStateException("CFGEdgeBuilder " + this
                    + " has already been built");
        }
        if (condition != null) {
            built = buildEdge(condition, evaluationResult, sourceBlock, targetBlock);
        } else {
            built = buildEdge(sourceBlock, targetBlock);
        }
        return built;
    }

    public static CFGEdge buildEdge(CFGBasicBlock source, CFGBasicBlock target) {
        return buildEdge(null, true, source, target);
    }

    public static CFGEdge buildEdge(Expression condition, CFGBasicBlock source,
            CFGBasicBlock target) {
        return buildEdge(condition, true, source, target);
    }

    public static CFGEdge buildEdge(Expression condition,
            boolean evaluationResult, CFGBasicBlock source, CFGBasicBlock target) {
        final CFGEdge edge = new CFGEdge(condition, evaluationResult, source,
                target);
        source.addOutgoingEdge(edge);
        target.addIncomingEdge(edge);
        return edge;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("EDGE_BUILDER[");
        this.sourceBlock.appendDotNodeLabel(sb);
        sb.append(" -> ");
        if (this.targetBlock != null) {
            this.targetBlock.appendDotNodeLabel(sb);
        } else {
            sb.append("?");
        }
        return sb.append("]").toString();
    }
}
