import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int } from 'type-graphql'
import { Post, User, Comment, PostCategory, PostFilterInput, PostSortInput, PostConnection } from '../../schema'
import { BaseResolver } from './base-resolver'
import { Context } from './types'
import { PRISMA_INCLUDES } from '../utils/resolver-helpers'

@Resolver(() => Post)
export class PostResolver extends BaseResolver {
	private readonly ALLOWED_FILTER_FIELDS = ['title', 'published', 'createdAt']
	private readonly ALLOWED_SORT_FIELDS = ['createdAt', 'updatedAt', 'viewCount']

	@Query(() => PostConnection)
	async posts(
		@Arg('filter', () => PostFilterInput, { nullable: true }) filter: PostFilterInput | null,
		@Arg('sort', () => PostSortInput, { nullable: true }) sort: PostSortInput | null,
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context,
	): Promise<PostConnection> {
		return this.buildRelayConnection<Post>(
			prisma,
			'post',
			{ filter, sort, first, after, last, before },
			this.ALLOWED_FILTER_FIELDS,
			this.ALLOWED_SORT_FIELDS,
			PRISMA_INCLUDES.POST,
		)
	}

	@Query(() => Post, { nullable: true })
	async post(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.findUnique<Post>(prisma, 'post', { id }, PRISMA_INCLUDES.POST)
	}

	@Mutation(() => Post)
	async createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
		@Ctx() { prisma }: Context,
	): Promise<Post> {
		return this.create<Post>(prisma, 'post', { title, content, published, authorId }, PRISMA_INCLUDES.POST)
	}

	@Mutation(() => Post, { nullable: true })
	async publishPost(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.update<Post>(prisma, 'post', { id }, { published: true }, PRISMA_INCLUDES.POST)
	}

	@FieldResolver(() => User)
	async author(@Root() post: Post, @Ctx() { prisma }: Context): Promise<User | null> {
		return this.findUnique<User>(prisma, 'user', { id: post.authorId })
	}

	@FieldResolver(() => [PostCategory])
	async categories(@Root() post: Post, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return this.findMany<PostCategory>(prisma, 'categoryOnPost', { where: { postId: post.id } })
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return this.findMany<Comment>(prisma, 'comment', {
			where: { postId: post.id },
			include: PRISMA_INCLUDES.COMMENT,
		})
	}
}
