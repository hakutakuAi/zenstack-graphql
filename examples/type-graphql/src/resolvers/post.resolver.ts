import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Post, User, Comment, PostCategory, PostFilterInput, PostSortInput, PostConnection, PostQueryArgs } from '../../schema'
import type { Context } from './types'
import { ConnectionBuilder, USER_INCLUDES, COMMENT_INCLUDES, POSTCATEGORY_INCLUDES, POST_INCLUDES } from '../../schema-helpers'

@Resolver(() => Post)
export class PostResolver {
	@Query(() => PostConnection)
	async posts(
		@Arg('filter', () => PostFilterInput, { nullable: true }) filter: PostFilterInput | undefined,
		@Arg('sort', () => PostSortInput, { nullable: true }) sort: PostSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<PostConnection> {
		const args: PostQueryArgs = { filter, sort, first, after, last, before }
		const config = ConnectionBuilder.buildPostConnectionConfig(args, info)

		const items = await prisma.post.findMany(config.findManyOptions)
		const totalCount = await prisma.post.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as PostConnection
	}

	@Query(() => Post, { nullable: true })
	async post(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({ where: { id }, include: POST_INCLUDES }) as Post | null
	}

	@Mutation(() => Post)
	async createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
		@Ctx() { prisma }: Context,
	): Promise<Post> {
		return await prisma.post.create({ data: { title, content, published, authorId }, include: POST_INCLUDES }) as Post
	}

	@Mutation(() => Post, { nullable: true })
	async publishPost(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.update({ where: { id }, data: { published: true }, include: POST_INCLUDES }) as Post | null
	}

	@FieldResolver(() => User)
	async author(@Root() post: Post, @Ctx() { prisma }: Context): Promise<User | null> {
		return await prisma.user.findUnique({ where: { id: post.authorId }, include: USER_INCLUDES }) as User | null
	}

	@FieldResolver(() => [PostCategory])
	async categories(@Root() post: Post, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return await prisma.categoryOnPost.findMany({ where: { postId: post.id }, include: POSTCATEGORY_INCLUDES }) as PostCategory[]
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return await prisma.comment.findMany({
			where: { postId: post.id },
			include: COMMENT_INCLUDES,
		}) as Comment[]
	}
}
