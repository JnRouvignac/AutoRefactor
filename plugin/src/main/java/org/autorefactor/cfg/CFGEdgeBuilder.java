package org.autorefactor.cfg;

import org.eclipse.jdt.core.dom.Expression;

import static org.autorefactor.util.Utils.*;

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
        final Boolean equal = basicEqual(this, obj);
        if (equal != null) {
            return equal;
        }
        final CFGEdgeBuilder other = (CFGEdgeBuilder) obj;
        return equal(condition, other.condition)
                && equal(evaluationResult, other.evaluationResult)
                && equal(sourceBlock, other.sourceBlock)
                && equal(targetBlock, other.targetBlock);
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
