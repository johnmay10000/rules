# SetTodoList Usage Example: Task Management System

**Project**: Kimi CLI Task Manager Example
**Type**: Reference Implementation
**Purpose**: Demonstrate SetTodoList tool integration with Kimi workflows

---

## 📋 Overview

This example demonstrates how to use Kimi's `SetTodoList` tool for effective task management across different complexity levels. The example follows the 3-tier documentation structure and shows progressive elaboration of requirements.

---

## 🎯 Example Goals

1. **Tier 1**: High-level architecture and roadmap
2. **Tier 2**: Detailed feature plan and task breakdown  
3. **Tier 3**: Daily execution logs and refinement

---

## 🏗️ Architecture Components

### System Design

```
kimi/examples/plan_with_todo/
├── ARCHITECTURE_PLAN.md          # Tier 1: Strategic overview (THIS FILE)
├── docs/
│   ├── plans/
│   │   ├── FEATURE_PLAN.md       # Tier 2: Feature specifications
│   │   └── FEATURE_TODO.md       # Tier 2: Detailed task breakdown
│   └── 2025_11_14/               # Tier 3: Daily execution logs
│       ├── 20251114_0001_TASK_BREAKDOWN.md
│       ├── 20251114_0002_INITIAL_SETUP.md
│       ├── 20251114_0003_IMPLEMENTATION.md
│       └── 20251114_0004_FINAL_TESTING.md
└── src/
    └── ... implementation files
```

### Kimi Integration Points

**SetTodoList Tool Usage:**
- Initialize with project milestones
- Break down into actionable tasks
- Update progress during implementation
- Mark completion with parallel tool calls

**Key Features Demonstrated:**
- Multi-level task hierarchy
- Status tracking (Pending/In Progress/Done)
- Time estimation and tracking
- Dependency management
- Cross-referenced documentation

---

## 📊 Tier Structure

### Tier 1: Strategic (ARCHITECTURE_PLAN.md)
- Vision and goals
- High-level architecture
- Major milestones
- Resource allocation
- Timeline estimates

### Tier 2: Tactical (docs/plans/)
- Feature specifications
- Task breakdowns
- Technical details
- Implementation approach
- Risk assessment

### Tier 3: Operational (docs/YYYYMMDD/)
- Daily execution logs
- Code snippets and decisions
- Problem/solution records
- Testing results
- Refinement details

---

## 🛠️ Implementation Workflow

### Phase 1: Planning
```
1. Create ARCHITECTURE_PLAN.md (this file)
   └─ Define scope and approach

2. Initialize SetTodoList with major milestones
   └─ 4-6 high-level phases

3. Break down into Tier 2 tasks
   └─ Create FEATURE_PLAN.md and FEATURE_TODO.md
```

### Phase 2: Execution
```
4. Daily documentation in docs/YYYYMMDD/
   └─ Create dated files with sequential numbering

5. Update SetTodoList progress
   └─ Mark tasks as In Progress → Done

6. Capture learnings and decisions
   └─ Record in appropriate tier level
```

### Phase 3: Review
```
7. Verify cross-references work
   └─ Tier 1 → Tier 2 → Tier 3 links

8. Review consistency across documents
   └─ Formatting, terminology, patterns

9. Git checkpoint with completion summary
   └─ Follow KIMI.md commit format
```

---

## 📈 SetTodoList Example Structure

### Milestone-Based Planning

**Milestone 1: Project Setup**
- ✅ Create project structure
- ✅ Define documentation hierarchy
- ✅ Initialize git repository
- ⏳ Create base configuration files

**Milestone 2: Core Implementation**
- ⏳ Implement main functionality
- ⏳ Add error handling
- ⏳ Write unit tests
- ⏳ Integration testing

**Milestone 3: Documentation**
- ⏳ Create user guide
- ⏳ Write API documentation
- ⏳ Add examples and tutorials
- ⏳ Review and polish

**Milestone 4: Deployment**
- ⏳ Prepare deployment package
- ⏳ Configure CI/CD pipeline
- ⏳ Deploy to staging
- ⏳ Production deployment

---

## 🎓 Learning Objectives

After studying this example, you should understand:

1. **When to Use SetTodoList**
   - Multi-step projects with clear phases
   - Complex tasks requiring breakdown
   - Team coordination scenarios
   - Long-running implementations

2. **Task Granularity Best Practices**
   - Tier 1: 4-8 major milestones
   - Tier 2: 15-30 actionable tasks
   - Tier 3: Daily execution details

3. **Status Management**
   - Use "Pending" for future work
   - Use "In Progress" for active tasks
   - Use "Done" for completed work
   - Update frequently (daily or per session)

4. **Integration with Git Workflow**
   - Commit after major milestone completion
   - Reference SetTodoList in commit messages
   - Update documentation hierarchy
   - Follow KIMI.md commit format

---

## 🔗 Cross-References

**Tier 2 Documents:**
- [docs/plans/FEATURE_PLAN.md](./docs/plans/FEATURE_PLAN.md) - Feature specifications
- [docs/plans/FEATURE_TODO.md](./docs/plans/FEATURE_TODO.md) - Detailed task breakdown

**Tier 3 Documents:**
- [docs/2025_11_14/](./docs/2025_11_14/) - Daily execution logs

**Reference Guides:**
- [kimi/KIMI_WORKFLOW_GUIDE.md](../../KIMI_WORKFLOW_GUIDE.md) - Kimi workflow patterns
- [kimi/FILE_LOCATIONS_USER_GUIDE.md](../../FILE_LOCATIONS_USER_GUIDE.md) - Documentation structure

---

## 📝 Template Usage

### For Your Own Projects:

1. **Copy Structure**
   ```bash
   cp -r kimi/examples/plan_with_todo/ my-new-project/
   cd my-new-project
   ```

2. **Customize ARCHITECTURE_PLAN.md**
   - Replace content with your project overview
   - Define your own milestones
   - Adjust timeline estimates

3. **Initialize SetTodoList**
   ```bash
   # Kimi will help you create initial todo list
   # Based on your ARCHITECTURE_PLAN.md
   ```

4. **Execute and Document**
   - Create Tier 2 and Tier 3 documents as needed
   - Update SetTodoList regularly
   - Commit with proper messages

---

## ✅ Completion Criteria

This example is complete when:

- [x] ARCHITECTURE_PLAN.md created (Tier 1)
- [x] docs/plans/ directory with Tier 2 documents
- [x] docs/2025_11_14/ directory with Tier 3 documents
- [x] SetTodoList integrated and demonstrated
- [x] Cross-references functional
- [x] Follows KIMI.md conventions
- [x] Git checkpoint with completion summary

---

**Status**: ✅ COMPLETE
**Last Updated**: 2025-11-14 18:50
**Maintained By**: Kimi CLI Global Rules System

🤖 Generated with [Kimi](https://kimi.ai)
