import { Resolver, Query, FieldResolver, Root, Ctx, Arg, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { PostCategory, Post, Category, PostCategoryConnection, PostCategoryQueryArgs } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { ConnectionBuilder, POST_INCLUDES, CATEGORY_INCLUDES } from '../../schema-helpers'

@Resolver(() => PostCategory)
export class PostCategoryResolver extends BaseResolver {
	@Query(() => PostCategoryConnection)
	async postCategories(
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<PostCategoryConnection> {
		const args: PostCategoryQueryArgs = { first, after, last, before }
		return ConnectionBuilder.buildPostCategoryConnection(prisma, args, info)
	}

	@FieldResolver(() => Post)
	async post(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.findUnique<Post>(prisma, 'post', { id: postCategory.postId }, POST_INCLUDES)
	}

	@FieldResolver(() => Category)
	async category(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Category | null> {
		return this.findUnique<Category>(prisma, 'category', { id: postCategory.categoryId }, CATEGORY_INCLUDES)
	}
}
