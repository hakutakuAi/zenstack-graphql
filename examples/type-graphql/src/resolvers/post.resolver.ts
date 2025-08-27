import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Post, User, Comment, PostCategory, PostFilterInput, PostSortInput, PostConnection, PostQueryArgs } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { ConnectionBuilder, USER_INCLUDES, COMMENT_INCLUDES, POSTCATEGORY_INCLUDES, POST_INCLUDES } from '../../schema-helpers'

@Resolver(() => Post)
export class PostResolver extends BaseResolver {
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
		return ConnectionBuilder.buildPostConnection(prisma, args, info)
	}

	@Query(() => Post, { nullable: true })
	async post(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.findUnique<Post>(prisma, 'post', { id }, POST_INCLUDES)
	}

	@Mutation(() => Post)
	async createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
		@Ctx() { prisma }: Context,
	): Promise<Post> {
		return this.create<Post>(prisma, 'post', { title, content, published, authorId }, POST_INCLUDES)
	}

	@Mutation(() => Post, { nullable: true })
	async publishPost(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.update<Post>(prisma, 'post', { id }, { published: true }, POST_INCLUDES)
	}

	@FieldResolver(() => User)
	async author(@Root() post: Post, @Ctx() { prisma }: Context): Promise<User | null> {
		return this.findUnique<User>(prisma, 'user', { id: post.authorId }, USER_INCLUDES)
	}

	@FieldResolver(() => [PostCategory])
	async categories(@Root() post: Post, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return this.findMany<PostCategory>(prisma, 'categoryOnPost', { where: { postId: post.id }, include: POSTCATEGORY_INCLUDES })
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return this.findMany<Comment>(prisma, 'comment', {
			where: { postId: post.id },
			include: COMMENT_INCLUDES,
		})
	}
}
