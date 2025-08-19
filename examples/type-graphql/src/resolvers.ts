import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int } from 'type-graphql'
import { PrismaClient } from '@prisma/client'
import {
	User, Post, Category, Comment, PostCategory,
	UserFilterInput, PostFilterInput, CategoryFilterInput, CommentFilterInput,
	UserSortInput, PostSortInput, CategorySortInput, CommentSortInput,
	UserConnection, PostConnection, CategoryConnection, CommentConnection,
	UserEdge, PostEdge, CategoryEdge, CommentEdge,
	PageInfo, PaginationInput, SortDirection
} from '../schema'

export interface Context {
	prisma: PrismaClient
}

@Resolver(() => User)
export class UserResolver {
	@Query(() => [User])
	async users(@Ctx() { prisma }: Context): Promise<User[]> {
		return (await prisma.user.findMany()) as User[]
	}

	@Query(() => [User])
	async usersFiltered(
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | null,
		@Ctx() { prisma }: Context
	): Promise<User[]> {
		const where = this.buildUserWhereClause(filter)
		return (await prisma.user.findMany({ where })) as User[]
	}

	@Query(() => [User])
	async usersSorted(
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | null,
		@Ctx() { prisma }: Context
	): Promise<User[]> {
		const orderBy = this.buildUserOrderByClause(sort)
		return (await prisma.user.findMany({ orderBy })) as User[]
	}

	@Query(() => UserConnection)
	async usersConnection(
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context
	): Promise<UserConnection> {
		return this.buildUserConnection({ first, after, last, before }, prisma)
	}

	@Query(() => User, { nullable: true })
	async user(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({
			where: { id },
		})) as User | null
	}

	@Mutation(() => User)
	async createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<User> {
		return (await prisma.user.create({
			data: {
				name,
				email,
				bio,
			},
		})) as User
	}

	@FieldResolver(() => [Post])
	async posts(@Root() user: User, @Ctx() { prisma }: Context): Promise<Post[]> {
		return await prisma.post.findMany({
			where: { authorId: user.id },
		})
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() user: User, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany({
			where: { authorId: user.id },
		})
	}

	private buildUserWhereClause(filter: UserFilterInput | null): any {
		if (!filter) return {}

		const where: any = {}

		if (filter.email) {
			if (filter.email.equals) where.email = { equals: filter.email.equals }
			if (filter.email.contains) where.email = { contains: filter.email.contains }
			if (filter.email.startsWith) where.email = { startsWith: filter.email.startsWith }
			if (filter.email.endsWith) where.email = { endsWith: filter.email.endsWith }
			if (filter.email.in) where.email = { in: filter.email.in }
			if (filter.email.notIn) where.email = { notIn: filter.email.notIn }
		}

		if (filter.name) {
			if (filter.name.equals) where.name = { equals: filter.name.equals }
			if (filter.name.contains) where.name = { contains: filter.name.contains }
			if (filter.name.startsWith) where.name = { startsWith: filter.name.startsWith }
			if (filter.name.endsWith) where.name = { endsWith: filter.name.endsWith }
		}

		if (filter.createdAt) {
			if (filter.createdAt.equals) where.createdAt = { equals: filter.createdAt.equals }
			if (filter.createdAt.gt) where.createdAt = { gt: filter.createdAt.gt }
			if (filter.createdAt.lt) where.createdAt = { lt: filter.createdAt.lt }
		}

		if (filter.AND) where.AND = filter.AND.map(f => this.buildUserWhereClause(f))
		if (filter.OR) where.OR = filter.OR.map(f => this.buildUserWhereClause(f))

		return where
	}

	private buildUserOrderByClause(sort: UserSortInput | null): any {
		if (!sort) return {}

		const orderBy: any = {}
		if (sort.createdAt) orderBy.createdAt = sort.createdAt.toLowerCase()
		if (sort.updatedAt) orderBy.updatedAt = sort.updatedAt.toLowerCase()

		return orderBy
	}

	private async buildUserConnection(
		pagination: { first: number | null; after: string | null; last: number | null; before: string | null },
		prisma: PrismaClient
	): Promise<UserConnection> {
		const { first, after, last, before } = pagination

		let take = first || last || 10
		if (last) take = -take

		const cursor = after || before ? { id: after || before } : undefined
		const skip = cursor ? 1 : 0

		const users = (await prisma.user.findMany({
			take,
			skip,
			cursor,
			orderBy: { id: 'asc' }
		})) as User[]

		const totalCount = await prisma.user.count()

		const edges: UserEdge[] = users.map((user, index) => ({
			node: user,
			cursor: user.id
		}))

		const hasNextPage = first ? users.length === first : false
		const hasPreviousPage = last ? users.length === Math.abs(last) : false

		const pageInfo: PageInfo = {
			hasNextPage,
			hasPreviousPage,
			startCursor: edges[0]?.cursor,
			endCursor: edges[edges.length - 1]?.cursor
		}

		return {
			pageInfo,
			edges,
			totalCount
		}
	}
}

@Resolver(() => Post)
export class PostResolver {
	@Query(() => [Post])
	async posts(@Ctx() { prisma }: Context): Promise<Post[]> {
		return await prisma.post.findMany()
	}

	@Query(() => [Post])
	async postsFiltered(
		@Arg('filter', () => PostFilterInput, { nullable: true }) filter: PostFilterInput | null,
		@Ctx() { prisma }: Context
	): Promise<Post[]> {
		const where = this.buildPostWhereClause(filter)
		return await prisma.post.findMany({ where })
	}

	@Query(() => [Post])
	async postsSorted(
		@Arg('sort', () => PostSortInput, { nullable: true }) sort: PostSortInput | null,
		@Ctx() { prisma }: Context
	): Promise<Post[]> {
		const orderBy = this.buildPostOrderByClause(sort)
		return await prisma.post.findMany({ orderBy })
	}

	@Query(() => [Post])
	async postsFilteredAndSorted(
		@Arg('filter', () => PostFilterInput, { nullable: true }) filter: PostFilterInput | null,
		@Arg('sort', () => PostSortInput, { nullable: true }) sort: PostSortInput | null,
		@Ctx() { prisma }: Context
	): Promise<Post[]> {
		const where = this.buildPostWhereClause(filter)
		const orderBy = this.buildPostOrderByClause(sort)
		return await prisma.post.findMany({ where, orderBy })
	}

	@Query(() => PostConnection)
	async postsConnection(
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context
	): Promise<PostConnection> {
		return this.buildPostConnection({ first, after, last, before }, prisma)
	}

	@Query(() => Post, { nullable: true })
	async post(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({
			where: { id },
		})
	}

	@Mutation(() => Post)
	async createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
		@Ctx() { prisma }: Context,
	): Promise<Post> {
		return await prisma.post.create({
			data: {
				title,
				content,
				published,
				authorId,
			},
		})
	}

	@Mutation(() => Post, { nullable: true })
	async publishPost(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.update({
			where: { id },
			data: { published: true },
		})
	}

	@FieldResolver(() => User)
	async author(@Root() post: Post, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({
			where: { id: post.authorId },
		})) as User | null
	}

	@FieldResolver(() => [PostCategory])
	async categories(@Root() post: Post, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return await prisma.categoryOnPost.findMany({
			where: { postId: post.id },
		}) as PostCategory[]
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany({
			where: { postId: post.id },
		})
	}

	private buildPostWhereClause(filter: PostFilterInput | null): any {
		if (!filter) return {}

		const where: any = {}

		if (filter.title) {
			if (filter.title.equals) where.title = { equals: filter.title.equals }
			if (filter.title.contains) where.title = { contains: filter.title.contains }
			if (filter.title.startsWith) where.title = { startsWith: filter.title.startsWith }
			if (filter.title.endsWith) where.title = { endsWith: filter.title.endsWith }
		}

		if (filter.published) {
			if (filter.published.equals !== undefined) where.published = { equals: filter.published.equals }
		}

		if (filter.createdAt) {
			if (filter.createdAt.equals) where.createdAt = { equals: filter.createdAt.equals }
			if (filter.createdAt.gt) where.createdAt = { gt: filter.createdAt.gt }
			if (filter.createdAt.lt) where.createdAt = { lt: filter.createdAt.lt }
		}

		if (filter.AND) where.AND = filter.AND.map(f => this.buildPostWhereClause(f))
		if (filter.OR) where.OR = filter.OR.map(f => this.buildPostWhereClause(f))

		return where
	}

	private buildPostOrderByClause(sort: PostSortInput | null): any {
		if (!sort) return {}

		const orderBy: any = {}
		if (sort.createdAt) orderBy.createdAt = sort.createdAt.toLowerCase()
		if (sort.updatedAt) orderBy.updatedAt = sort.updatedAt.toLowerCase()
		if (sort.viewCount) orderBy.viewCount = sort.viewCount.toLowerCase()

		return orderBy
	}

	private async buildPostConnection(
		pagination: { first: number | null; after: string | null; last: number | null; before: string | null },
		prisma: PrismaClient
	): Promise<PostConnection> {
		const { first, after, last, before } = pagination

		let take = first || last || 10
		if (last) take = -take

		const cursor = after || before ? { id: after || before } : undefined
		const skip = cursor ? 1 : 0

		const posts = await prisma.post.findMany({
			take,
			skip,
			cursor,
			orderBy: { id: 'asc' }
		})

		const totalCount = await prisma.post.count()

		const edges: PostEdge[] = posts.map((post) => ({
			node: post as Post,
			cursor: post.id
		}))

		const hasNextPage = first ? posts.length === first : false
		const hasPreviousPage = last ? posts.length === Math.abs(last) : false

		const pageInfo: PageInfo = {
			hasNextPage,
			hasPreviousPage,
			startCursor: edges[0]?.cursor,
			endCursor: edges[edges.length - 1]?.cursor
		}

		return {
			pageInfo,
			edges,
			totalCount
		}
	}
}

@Resolver(() => Category)
export class CategoryResolver {
	@Query(() => [Category])
	async categories(@Ctx() { prisma }: Context): Promise<Category[]> {
		return (await prisma.category.findMany()) as Category[]
	}

	@Query(() => Category, { nullable: true })
	async category(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Category | null> {
		return (await prisma.category.findUnique({
			where: { id },
		})) as Category | null
	}

	@Mutation(() => Category)
	async createCategory(
		@Arg('name', () => String) name: string,
		@Arg('description', () => String, { nullable: true }) description: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<Category> {
		return (await prisma.category.create({
			data: {
				name,
				description,
			},
		})) as Category
	}

	@FieldResolver(() => [PostCategory])
	async posts(@Root() category: Category, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return await prisma.categoryOnPost.findMany({
			where: { categoryId: category.id },
		}) as PostCategory[]
	}
}

@Resolver(() => Comment)
export class CommentResolver {
	@Query(() => [Comment])
	async comments(@Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany()
	}

	@Query(() => [Comment])
	async commentsFiltered(
		@Arg('filter', () => CommentFilterInput, { nullable: true }) filter: CommentFilterInput | null,
		@Ctx() { prisma }: Context
	): Promise<Comment[]> {
		const where = this.buildCommentWhereClause(filter)
		return await prisma.comment.findMany({ where })
	}

	@Query(() => [Comment])
	async commentsSorted(
		@Arg('sort', () => CommentSortInput, { nullable: true }) sort: CommentSortInput | null,
		@Ctx() { prisma }: Context
	): Promise<Comment[]> {
		const orderBy = this.buildCommentOrderByClause(sort)
		return await prisma.comment.findMany({ orderBy })
	}

	@Query(() => CommentConnection)
	async commentsConnection(
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context
	): Promise<CommentConnection> {
		return this.buildCommentConnection({ first, after, last, before }, prisma)
	}

	@Mutation(() => Comment)
	async createComment(
		@Arg('content', () => String) content: string,
		@Arg('postId', () => String) postId: string,
		@Arg('authorId', () => String) authorId: string,
		@Ctx() { prisma }: Context,
	): Promise<Comment> {
		return await prisma.comment.create({
			data: {
				content,
				postId,
				authorId,
			},
		})
	}

	@FieldResolver(() => User)
	async author(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({
			where: { id: comment.authorId },
		})) as User | null
	}

	@FieldResolver(() => Post)
	async post(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({
			where: { id: comment.postId },
		})
	}

	private buildCommentWhereClause(filter: CommentFilterInput | null): any {
		if (!filter) return {}

		const where: any = {}

		if (filter.AND) where.AND = filter.AND.map(f => this.buildCommentWhereClause(f))
		if (filter.OR) where.OR = filter.OR.map(f => this.buildCommentWhereClause(f))

		return where
	}

	private buildCommentOrderByClause(sort: CommentSortInput | null): any {
		if (!sort) return {}

		const orderBy: any = {}
		if (sort.createdAt) orderBy.createdAt = sort.createdAt.toLowerCase()

		return orderBy
	}

	private async buildCommentConnection(
		pagination: { first: number | null; after: string | null; last: number | null; before: string | null },
		prisma: PrismaClient
	): Promise<CommentConnection> {
		const { first, after, last, before } = pagination

		let take = first || last || 10
		if (last) take = -take

		const cursor = after || before ? { id: after || before } : undefined
		const skip = cursor ? 1 : 0

		const comments = await prisma.comment.findMany({
			take,
			skip,
			cursor,
			orderBy: { id: 'asc' }
		})

		const totalCount = await prisma.comment.count()

		const edges: CommentEdge[] = comments.map((comment) => ({
			node: comment as Comment,
			cursor: comment.id
		}))

		const hasNextPage = first ? comments.length === first : false
		const hasPreviousPage = last ? comments.length === Math.abs(last) : false

		const pageInfo: PageInfo = {
			hasNextPage,
			hasPreviousPage,
			startCursor: edges[0]?.cursor,
			endCursor: edges[edges.length - 1]?.cursor
		}

		return {
			pageInfo,
			edges,
			totalCount
		}
	}
}

@Resolver(() => PostCategory)
export class PostCategoryResolver {
	@FieldResolver(() => Post)
	async post(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({
			where: { id: postCategory.postId },
		})
	}

	@FieldResolver(() => Category)
	async category(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Category | null> {
		return await prisma.category.findUnique({
			where: { id: postCategory.categoryId },
		}) as Category | null
	}
}
